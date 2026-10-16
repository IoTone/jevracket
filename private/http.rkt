#lang racket/base

;; private/http.rkt — transport and the retry loop.
;;
;; Racket's `http-sendrecv` has no timeout of its own, so every attempt runs in a
;; thread that the caller abandons on the deadline. Without this a single stalled
;; socket hangs the calling thread forever, which in a scheduler-driven host means
;; a job that holds its slot until someone restarts the process.
;;
;; Retry defaults match the vendor SDK so behaviour does not depend on which
;; client you used: 2 retries after the first attempt, 0.5s doubling to 5s, 25%
;; jitter, retry on 408/429/5xx (which covers 529 Overloaded) and on connection
;; and timeout failures, honour `Retry-After`, and give up after 30s overall.
;;
;; Two rules the loop keeps that are easy to get wrong:
;;   • `Retry-After` WINS over the computed backoff, and is clamped to what is
;;     left of the deadline. A server telling you when to come back knows more
;;     than your exponent does.
;;   • The deadline is checked BEFORE sleeping, so a client with a 30s budget
;;     never sleeps 5s only to raise a timeout on waking.

(require racket/list racket/port racket/string net/http-client json "error.rkt")

(provide (struct-out retry-policy)
         default-retry-policy
         retry-status?
         http-request/retry
         DEFAULT-TIMEOUT DEFAULT-DEADLINE USER-AGENT)

(define DEFAULT-TIMEOUT 10.0)    ; seconds, one attempt
(define DEFAULT-DEADLINE 30.0)   ; seconds, all attempts together
(define USER-AGENT "jev-racket/0.2.0")

;; statuses : (or/c (listof exact-integer?) (-> exact-integer? boolean?))
(struct retry-policy (max-retries backoff-initial backoff-max jitter statuses
                      respect-retry-after? retry-connection? deadline)
  #:transparent)

(define default-retry-policy
  (retry-policy 2 0.5 5.0 0.25 retryable-api-status? #t #t DEFAULT-DEADLINE))

(define (retry-status? p code)
  (define s (retry-policy-statuses p))
  (if (procedure? s) (and (s code) #t) (and (memv code s) #t)))

;; ---- URL ----------------------------------------------------------------------
;; Deliberately a regexp rather than net/url: the only URLs this client builds are
;; a base plus a fixed path, and net/url would add a dependency and a reassembly
;; step for a string we already have.
(define (url-parts s)
  (define m (regexp-match #px"^(https?)://([^/:?#]+)(?::([0-9]+))?([/?].*)?$" s))
  (unless m (error 'jev "malformed base URL: ~a" s))
  (define ssl? (string=? (list-ref m 1) "https"))
  (values (list-ref m 2)
          (if (list-ref m 3) (string->number (list-ref m 3)) (if ssl? 443 80))
          ssl?
          (let ([p (list-ref m 4)]) (if (or (not p) (string=? p "")) "/" p))))

(define (status-code line)
  (define m (regexp-match #px"^HTTP/[0-9.]+ +([0-9]{3})" (bytes->string/utf-8 line #\?)))
  (unless m (error 'jev "unparseable status line: ~a" line))
  (string->number (list-ref m 1)))

;; Lower-cased names, so a lookup never depends on the server's capitalization.
(define (parse-headers hs)
  (for/list ([h (in-list hs)])
    (define s (bytes->string/utf-8 h #\?))
    (define i (for/first ([c (in-string s)] [n (in-naturals)] #:when (char=? c #\:)) n))
    (if i
        (cons (string-downcase (string-trim (substring s 0 i))) (string-trim (substring s (add1 i))))
        (cons (string-downcase (string-trim s)) ""))))

;; ---- one attempt, with a deadline ---------------------------------------------

;; Runs the exchange in its own thread and abandons it on timeout. The body is
;; read to completion INSIDE that thread: handing a live port back across the
;; timeout boundary would just move the stall to the first read.
(define (send-once method url headers data timeout)
  (define-values (host port ssl? path) (url-parts url))
  (define ch (make-channel))
  ;; The attempt runs under its own custodian so abandoning it on timeout also
  ;; CLOSES the socket it opened. Killing the thread alone leaks the connection
  ;; until a GC notices, which under load is how a client runs out of file
  ;; descriptors long before it runs out of patience.
  (define cust (make-custodian))
  (define t
    (parameterize ([current-custodian cust])
     (thread
     (lambda ()
       (channel-put
        ch
        (with-handlers ([(lambda (_) #t) (lambda (e) (cons 'exn e))])
          (define-values (status hs in)
            (http-sendrecv host path #:ssl? ssl? #:port port #:method method
                           #:headers headers #:data data))
          (define body (port->string in))
          (close-input-port in)
          (list 'ok (status-code status) (parse-headers hs) body)))))))
  (define r (sync/timeout timeout ch))
  (cond
    [(not r)
     (kill-thread t)
     (custodian-shutdown-all cust)
     (raise (exn:fail:jev:timeout
             (format "jev: ~a ~a timed out after ~as" method url timeout)
             (current-continuation-marks) timeout))]
    [(eq? (car r) 'exn)
     (custodian-shutdown-all cust)
     (define e (cdr r))
     (raise (exn:fail:jev:connection
             (format "jev: ~a ~a failed: ~a" method url
                     (if (exn? e) (exn-message e) e))
             (current-continuation-marks)))]
    [else
     (custodian-shutdown-all cust)
     (values (list-ref r 1) (list-ref r 2) (list-ref r 3))]))

;; ---- the retry loop -----------------------------------------------------------

(define (backoff-delay p attempt)
  (define base (min (retry-policy-backoff-max p)
                    (* (retry-policy-backoff-initial p) (expt 2 attempt))))
  (define j (retry-policy-jitter p))
  ;; ±j of the base, so a fleet of clients that failed together does not come
  ;; back in lockstep and re-trigger the same 429.
  (max 0.0 (* base (+ 1.0 (* j (- (* 2.0 (random)) 1.0))))))

;; -> (values status headers body-string)
;; Raises exn:fail:jev:* on a final failure. `retry-after-ms` from a 429 wins over
;; the computed backoff when the policy respects it.
(define (http-request/retry method url headers data
                            #:timeout [timeout DEFAULT-TIMEOUT]
                            #:policy [p default-retry-policy]
                            #:endpoint [endpoint #f])
  (define ep (or endpoint (format "~a ~a" method url)))
  (define start (current-inexact-milliseconds))
  (define deadline (retry-policy-deadline p))
  (define (left) (and deadline (- deadline (/ (- (current-inexact-milliseconds) start) 1000.0))))
  (let loop ([attempt 0])
    (define (retry-or-raise raise-it! suggested-ms)
      (define remaining (left))
      (cond
        [(>= attempt (retry-policy-max-retries p)) (raise-it!)]
        [(and remaining (<= remaining 0)) (raise-it!)]
        [else
         (define want (if (and suggested-ms (retry-policy-respect-retry-after? p))
                          (/ suggested-ms 1000.0)
                          (backoff-delay p attempt)))
         ;; Check the budget BEFORE sleeping: waiting out a delay we cannot afford
         ;; and then giving up wastes exactly that much of the caller's time.
         (define nap (if remaining (min want remaining) want))
         (cond [(and remaining (>= nap remaining)) (raise-it!)]
               [else (sleep nap) (loop (add1 attempt))])]))
    (define-values (status hs body)
      (with-handlers ([exn:fail:jev:connection?
                       (lambda (e)
                         (if (retry-policy-retry-connection? p)
                             (retry-or-raise (lambda () (raise e)) #f)
                             (raise e)))])
        (send-once method url headers data
                   (if (and (left) (< (left) timeout)) (max 0.001 (left)) timeout))))
    ;; A retried connection error re-enters `loop`, whose only non-raising exit is
    ;; a 2xx, so by here `status` is always a real status from a real response.
    (cond
      [(and (>= status 200) (< status 300)) (values status hs body)]
      [else
       (define parsed
         (with-handlers ([exn:fail? (lambda (_) body)])
           (if (string=? (string-trim body) "") #f (string->jsexpr body))))
       (define (raise-it!) (raise-api-error status parsed hs ep))
       (if (retry-status? p status)
           (retry-or-raise raise-it!
                           (let ([v (assoc "retry-after" hs)])
                             (and v (let ([n (string->number (string-trim (cdr v)))])
                                      (and n (real? n) (>= n 0)
                                           (inexact->exact (round (* n 1000))))))))
           (raise-it!))])))
