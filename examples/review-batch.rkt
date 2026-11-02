#lang racket/base

;; examples/review-batch.rkt — score a list of records, one request per record.
;;
;; Every question sees the WHOLE state, so a question about "this review" needs
;; a state that holds one review. Batching several records into one state would
;; blur them together. Asking several questions about one record in one request
;; is what saves round trips.
;;
;; State can be structured: each record goes over as a JSON object, and the
;; questions refer to its fields by name.

(require racket/format racket/list "../main.rkt" "laya.rkt")

(provide review-questions score-review)

(define reviews
  (list (hasheq 'product "kettle"     'stars 5 'text "Boils fast, quiet, looks great on the counter.")
        (hasheq 'product "kettle"     'stars 2 'text "Lid broke after a week. Customer service never replied.")
        (hasheq 'product "headphones" 'stars 4 'text "Great sound, a bit tight on my head after an hour.")
        (hasheq 'product "headphones" 'stars 1 'text "Left side died on day three. Want my money back.")
        (hasheq 'product "backpack"   'stars 3 'text "Fine. Does the job. Zipper feels cheap.")))

(define review-questions
  (list (cons "sentiment" (score "How positive the `text` of this review is"
                                 '("Very negative" "Negative" "Mixed" "Positive" "Very positive")))
        (cons "defect"    (noul "The `text` reports a product that broke or stopped working."))
        (cons "wants_refund" (noul "The `text` asks for money back."))))

(define (score-review rec) (ask laya rec review-questions))

(module+ main
  (define t0 (current-inexact-milliseconds))
  (define rows
    (for/list ([rec (in-list reviews)])
      (define r (score-review rec))
      (list (hash-ref rec 'product) (hash-ref rec 'stars)
            (answer-value (response-ref r "sentiment"))
            (answer-value (response-ref r "defect"))
            (answer-value (response-ref r "wants_refund")))))
  (define ms (- (current-inexact-milliseconds) t0))
  (printf "~a ~a ~a ~a ~a\n" (~a "product" #:min-width 11) "stars" "sentiment(0-4)" "defect" "refund")
  (for ([row (in-list rows)])
    (printf "~a ~a ~a ~a ~a\n"
            (~a (first row) #:min-width 11) (~a (second row) #:min-width 5)
            (~r (third row) #:precision '(= 2) #:min-width 14)
            (~r (fourth row) #:precision '(= 2) #:min-width 6)
            (~r (fifth row) #:precision '(= 2) #:min-width 6)))
  (printf "\n~a requests in ~a ms (~a ms each)\n"
          (length rows) (~r ms #:precision 0) (~r (/ ms (length rows)) #:precision 0)))
