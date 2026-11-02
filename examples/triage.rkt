
#lang racket/base

;; examples/triage.rkt — route one support message with three questions in a
;; single request: a noul (yes/no), a choice (one of a set) and a score (a rubric).
;;
;; In racket-mode: C-c C-c runs the `main` submodule below, then try
;;   (show (triage "My card was charged twice for one order."))
;; in the REPL.

(require racket/format "../main.rkt" "laya.rkt")

(provide triage show)

;; An association list, not a hash, so the questions reach the model in this order.
(define questions
  (list (cons "urgent" (noul "The message conveys time pressure."))
        (cons "team"   (choice "Which team should handle this message"
                               '(("billing"   . "Payments, invoicing, refunds, charges")
                                 ("technical" . "Bugs, outages, errors, integrations")
                                 ("account"   . "Logins, passwords, profile changes"))))
        (cons "anger"  (score "How angry the writer sounds"
                              '("Calm" "Annoyed" "Furious")))))

(define (triage msg) (ask laya msg questions))

(define (show r)
  (define team (response-ref r "team"))
  (printf "  urgent ~a   team ~a (~a)   anger ~a of 0–2\n"
          (~r (answer-value (response-ref r "urgent")) #:precision '(= 2))
          (answer-value team)
          (~r (answer-confidence team) #:precision '(= 2))
          (~r (answer-value (response-ref r "anger")) #:precision '(= 2))))

(module+ main
  (for ([msg (in-list
              '("Payouts have been failing for three days and I am losing sales."
                "My card was charged twice for one order."
                "I can't log in since I changed my email address."
                "The export button throws a 500 error. Not urgent, whenever you get to it."))])
    (printf "~a\n" msg)
    (show (triage msg))))
