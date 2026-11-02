#lang racket/base

;; examples/confidence-gate.rkt — act on confident answers, send the rest to a
;; human. The answer says WHAT; the confidence says whether to act on it.
;;
;; Laya reports two confidences for a choice or score:
;;   confidence        normalized entropy, Laya's own measure (what `ask` returns)
;;   x_jev_confidence  the number Jev's formula gives for the same probabilities
;; A threshold you tuned on TypeSafe means x_jev_confidence. `ask` only parses the
;; contract fields, so this example takes the raw body from `ask/raw`, reads the
;; extra field, and parses the same body with `parse-response` for the typed answer.

(require racket/format "../main.rkt" "laya.rkt")

(provide route)

(define THRESHOLD 0.85)  ; illustrative. Tune it on your own labelled messages.

(define questions
  (list (cons "intent"
              (choice "What does the writer want"
                      '(("refund"   . "Money back for a purchase")
                        ("cancel"   . "End a subscription or account")
                        ("question" . "Information, no action needed")
                        ("complaint" . "Unhappy, but no specific request"))))))

;; -> (values decision intent jev-confidence laya-confidence)
(define (route msg)
  (define raw (ask/raw laya msg questions))
  (define a   (response-ref (parse-response raw) "intent"))
  (define extra (hash-ref (hash-ref (hash-ref raw 'answers) 'intent) 'x_jev_confidence #f))
  ;; a TypeSafe body, or LAYA_JEV_STRICT=1, has no extra field: fall back
  (define conf (or extra (answer-confidence a)))
  (values (if (>= conf THRESHOLD) 'act 'human)
          (answer-value a) conf (answer-confidence a)))

(module+ main
  (printf "threshold ~a on x_jev_confidence\n\n" THRESHOLD)
  (for ([msg (in-list
              '("Please refund order 1182, the item arrived broken."
                "Cancel my plan at the end of this month."
                "Do you ship to Canada?"
                "Honestly not sure this product is for me, it's been a frustrating week."))])
    (define-values (decision intent jev laya-conf) (route msg))
    (printf "~a  ~a  (jev ~a, laya ~a)\n    ~a\n"
            (if (eq? decision 'act) "ACT  " "HUMAN")
            (~a intent #:min-width 9)
            (~r jev #:precision '(= 2)) (~r laya-conf #:precision '(= 2))
            msg)))
