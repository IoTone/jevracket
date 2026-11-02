#lang racket/base

;; test/laya-smoke.rkt — jev against a running laya-serve or laya-mlx-serve.
;;
;; Not a unit test: it needs a Laya server, which is a local model, not a mock.
;; It pins the differences the README's "Self-hosting with Laya" section names, so a
;; Laya release that changes one of them fails here rather than in a host.
;;
;;   LAYA_HOST=127.0.0.1 LAYA_PORT=8765 LAYA_MODELS=english laya-mlx-serve &
;;   LAYA_BASE_URL=http://127.0.0.1:8765 racket test/laya-smoke.rkt
;;
;; LAYA_BASE_URL defaults to http://127.0.0.1:8000, laya-serve's own default port.
;; Checked against laya-mlx 0.6.0 (english checkpoint, Apple GPU).

(require racket/format "../main.rkt")

(define base (or (getenv "LAYA_BASE_URL") "http://127.0.0.1:8000"))
(define api? exn:fail:jev:api?)
(define api-status exn:fail:jev:api-status)
(define api-body exn:fail:jev:api-body)

(define fails 0)
(define (report name ok detail)
  (unless ok (set! fails (add1 fails)))
  (printf "~a  ~a  ~a\n" (if ok "PASS" "FAIL") name detail))
(define (status-of thunk)
  (with-handlers ([api? (lambda (e) (list (api-status e) (api-body e)))]
                  [exn:fail? (lambda (e) (list 'other (exn-message e)))])
    (thunk) (list 'ok #f)))

(define plain (make-client #:base-url base #:api-key "unused" #:compress? #f))
(define gz    (make-client #:base-url base #:api-key "unused"))
(define qs
  (list (cons "urgent" (noul "The message conveys time pressure."))
        (cons "team"   (choice "Which team should handle this"
                               '(("billing" . "Payments, invoicing, refunds")
                                 ("technical" . "Bugs, outages, integrations"))))
        (cons "anger"  (score "How angry the writer sounds" '("Calm" "Annoyed" "Furious")))))
(define state "Payouts have been failing for three days and I am losing sales.")

;; 1. the three primitives, uncompressed
(define r (ask plain state qs))
(report "three primitives parse" #t
        (format "model=~a urgent=~a team=~a(conf ~a) anger=~a"
                (response-model r) (answer-value (response-ref r "urgent"))
                (answer-value (response-ref r "team")) (answer-confidence (response-ref r "team"))
                (answer-value (response-ref r "anger"))))

;; 2. x_jev_confidence is present in the raw body
(define raw (ask/raw plain state qs))
(define team-raw (hash-ref (hash-ref raw 'answers) 'team))
(report "x_jev_confidence in ask/raw" (real? (hash-ref team-raw 'x_jev_confidence #f))
        (format "confidence=~a x_jev_confidence=~a" (hash-ref team-raw 'confidence)
                (hash-ref team-raw 'x_jev_confidence #f)))

;; 3. a body >= 1 KiB with compression ON
(define big (string-append state " " (make-string 1500 #\x)))
(define s3 (status-of (lambda () (ask gz big qs))))
(report "gzip body (default client) is refused with 400" (equal? (car s3) 400) (~s s3))
(define s3b (status-of (lambda () (ask plain big qs))))
(report "same body with #:compress? #f succeeds" (eq? (car s3b) 'ok) (~s s3b))

;; 4. a noul without instructions
(define s4 (status-of (lambda () (ask plain state (list (cons "due" (noul #:yes "the invoice is past due")))))))
(report "instructions-less noul is refused with 422" (equal? (car s4) 422) (~s s4))

;; 5. list-models
(define s5 (status-of (lambda () (list-models plain))))
(report "list-models is 404" (equal? (car s5) 404) (~s s5))

;; 6. pinning a checkpoint by name
(define r6 (ask plain state (list (cons "urgent" (noul "The message conveys time pressure."))) #:model "english"))
(report "#:model \"english\" accepted" #t (format "model=~a" (response-model r6)))

(exit (if (zero? fails) 0 1))
