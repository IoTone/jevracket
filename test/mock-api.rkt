#lang racket/base

;; test/mock-api.rkt — a scriptable HTTP/1.1 server for the client tests.
;;
;; NOT a test file: it blocks, which is why info.rkt keeps it out of `raco test`.
;; Testing the client against a real socket rather than a stubbed transport is
;; the whole point — the bugs worth catching here (a header the client forgets,
;; a Content-Length it never sends, a retry that reuses a dead connection) are
;; invisible to a mock that replaces `http-sendrecv`.

(require racket/tcp racket/port racket/string racket/list)

(provide start-mock! mock-stop! mock-url mock-port mock-requests mock-count
         recorded-method recorded-path recorded-headers recorded-body
         (struct-out recorded))

;; method/path/headers as they arrived; body as BYTES, because a compressed or
;; multipart body is not text. `recorded-body` answers it as a string for the
;; JSON cases, which is what most assertions read.
(struct recorded (method path headers body-bytes) #:transparent)
(define (recorded-body r) (bytes->string/utf-8 (recorded-body-bytes r) #\?))

;; `stopper` rather than `stop!`, so the accessor does not collide with the
;; procedure callers actually want.
(struct mock (port stopper reqs) #:transparent)

(define (mock-url m [path ""]) (format "http://127.0.0.1:~a~a" (mock-port m) path))
(define (mock-requests m) (reverse (unbox (mock-reqs m))))
(define (mock-count m) (length (unbox (mock-reqs m))))
(define (mock-stop! m) ((mock-stopper m)))

;; `respond` is (attempt-index recorded -> (list status headers-alist body-string))
;; or #f to hang forever, which is how the timeout case is expressed.
(define (start-mock! respond)
  (define listener (tcp-listen 0 8 #t "127.0.0.1"))
  (define port (call-with-values (lambda () (tcp-addresses listener #t)) (lambda vs (cadr vs))))
  (define reqs (box '()))
  (define alive (box #t))
  (define t
    (thread
     (lambda ()
       (let loop ()
         (define ok?
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (define-values (in out) (tcp-accept listener))
             (thread (lambda () (serve in out reqs respond)))
             #t))
         (when (and ok? (unbox alive)) (loop))))))
  (define (stop!)
    (set-box! alive #f)
    (with-handlers ([exn:fail? void]) (tcp-close listener))
    (kill-thread t))
  (mock port stop! reqs))

(define (serve in out reqs respond)
  (with-handlers ([exn:fail? (lambda (_) (close in out))])
    (define line (read-line in 'any))
    (when (string? line)
      (define parts (string-split line " "))
      (define hs
        (let loop ([acc '()])
          (define l (read-line in 'any))
          (if (or (eof-object? l) (string=? (string-trim l) "")) (reverse acc)
              (loop (cons (parse-header l) acc)))))
      (define len (cond [(assoc "content-length" hs) => (lambda (p) (string->number (cdr p)))] [else 0]))
      ;; bytes, not read-string: Content-Length counts bytes, and a multipart or
      ;; gzipped body is binary
      (define body (if (and len (> len 0)) (read-bytes len in) #""))
      (define r (recorded (if (pair? parts) (car parts) "")
                          (if (> (length parts) 1) (cadr parts) "")
                          hs (if (bytes? body) body #"")))
      (define n (length (unbox reqs)))
      (set-box! reqs (cons r (unbox reqs)))
      (cond
        [(not respond) (sleep 30) (close in out)]          ; the timeout case
        [else
         (define spec (respond n r))
         (cond
           [(not spec) (sleep 30) (close in out)]
           [else
            (define status (first spec))
            (define extra (second spec))
            (define b (string->bytes/utf-8 (third spec)))
            (fprintf out "HTTP/1.1 ~a ~a\r\n" status (reason status))
            (for ([p (in-list extra)]) (fprintf out "~a: ~a\r\n" (car p) (cdr p)))
            (fprintf out "Content-Length: ~a\r\n" (bytes-length b))
            (fprintf out "Connection: close\r\n\r\n")
            (write-bytes b out)
            (flush-output out)
            (close in out)])]))))

(define (close in out)
  (with-handlers ([exn:fail? void]) (flush-output out))
  (with-handlers ([exn:fail? void]) (close-output-port out))
  (with-handlers ([exn:fail? void]) (close-input-port in)))

(define (parse-header l)
  (define i (for/first ([c (in-string l)] [n (in-naturals)] #:when (char=? c #\:)) n))
  (if i (cons (string-downcase (string-trim (substring l 0 i))) (string-trim (substring l (add1 i))))
      (cons (string-downcase (string-trim l)) "")))

(define (reason s)
  (case s [(200) "OK"] [(400) "Bad Request"] [(401) "Unauthorized"] [(403) "Forbidden"]
          [(404) "Not Found"] [(422) "Unprocessable Entity"] [(429) "Too Many Requests"]
          [(500) "Internal Server Error"] [(529) "Overloaded"] [else "Status"]))
