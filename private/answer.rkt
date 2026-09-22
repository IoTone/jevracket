#lang racket/base

;; private/answer.rkt — typed answers, parsed strictly.
;;
;; Three answer structs mirroring the three question types, plus three generic
;; accessors (`answer-value`, `answer-confidence`, `answer-probabilities`) so a
;; caller that routes on confidence does not have to dispatch on the type first.
;;
;; Two places this is deliberately NOT a transcription of the wire:
;;
;;   • A Score's `legend` and `probabilities` come back keyed by level index as
;;     JSON STRINGS ("0", "1", "2"). They are integers — they index an ordered
;;     list the caller wrote — so they arrive here as integers. Leaving them as
;;     strings would push `(string->number (symbol->string k))` into every caller
;;     that wants the level above the winner.
;;   • A Choice's probability keys stay STRINGS, because they are option names the
;;     caller chose. Handing back the symbols that `read-json` happens to produce
;;     would be an artifact of the parser, not part of the contract.
;;
;; Everything else is strict. A 2xx that is missing `confidence`, or names a
;; `choice` that is not in its own distribution, raises exn:fail:jev:response with
;; the field path. See the note in error.rkt for why a half-built answer is worse
;; than a raised one.

(require racket/list json "error.rkt")

(provide (struct-out answer)
         (struct-out noul-answer)
         (struct-out choice-answer)
         (struct-out score-answer)
         (struct-out usage)
         (struct-out response)
         answer-value answer-confidence answer-probabilities
         response-ref response-ids
         parse-response)

;; type : 'noul | 'choice | 'score
(struct answer (type) #:transparent)

;; value : real in [0,1] — the probability that the answer is YES. Not a boolean:
;; 0.5 is a real thing for a Noul to say, and rounding it away at the library
;; boundary would throw away the only interesting part.
(struct noul-answer answer (value) #:transparent)

;; value         : string, the highest-probability option
;; probabilities : hash string -> real, every option, summing to 1
;; confidence    : real in [0,1], derived from the distribution
(struct choice-answer answer (value probabilities confidence) #:transparent)

;; value         : real, probability-weighted across levels; MAY land between two
;;                 of them (1.05 means "mostly level 1, leaning 2")
;; legend        : hash exact-nonnegative-integer -> string
;; probabilities : hash exact-nonnegative-integer -> real
;; confidence    : real in [0,1]
(struct score-answer answer (value legend probabilities confidence) #:transparent)

(struct usage (input-tokens output-tokens) #:transparent)

;; model   : the VERSIONED id that answered, e.g. "jev-1.13.0", even when the
;;           request asked for the alias "jev-latest". Log it: an alias moves
;;           under you, and confidence thresholds are tuned per version.
;; answers : hash string -> answer, keyed by the ids you sent
;; usage   : usage
(struct response (model answers usage) #:transparent)

(define (response-ref r id)
  (hash-ref (response-answers r) id
            (lambda () (error 'response-ref "no answer named ~s; this response has ~a"
                              id (let ([ks (response-ids r)])
                                   (if (null? ks) "none" (string-join* ks)))))))

(define (response-ids r) (sort (hash-keys (response-answers r)) string<?))

(define (string-join* xs) (apply string-append (add-between (map (lambda (s) (format "~s" s)) xs) ", ")))

;; ---- generic accessors --------------------------------------------------------

(define (answer-value a)
  (cond [(noul-answer? a)   (noul-answer-value a)]
        [(choice-answer? a) (choice-answer-value a)]
        [(score-answer? a)  (score-answer-value a)]
        [else (raise-argument-error 'answer-value "answer?" a)]))

;; #f for a Noul, and that is the API's shape, not an omission here: a Noul's
;; probability already IS its own certainty, so there is no second number to
;; report. Callers that gate on confidence should treat #f as "not applicable"
;; rather than as "unconfident".
(define (answer-confidence a)
  (cond [(noul-answer? a)   #f]
        [(choice-answer? a) (choice-answer-confidence a)]
        [(score-answer? a)  (score-answer-confidence a)]
        [else (raise-argument-error 'answer-confidence "answer?" a)]))

(define (answer-probabilities a)
  (cond [(noul-answer? a)   #f]
        [(choice-answer? a) (choice-answer-probabilities a)]
        [(score-answer? a)  (score-answer-probabilities a)]
        [else (raise-argument-error 'answer-probabilities "answer?" a)]))

;; ---- parsing ------------------------------------------------------------------

(define-struct ctx (status body headers endpoint) #:transparent)

(define (fail c path) (raise-response-error path (ctx-status c) (ctx-body c) (ctx-headers c) (ctx-endpoint c)))

;; `path` names the CONTAINING object, and the key is appended — so the top level
;; passes "" and gets "model" rather than "model.model".
(define (join path key)
  (if (string=? path "") (symbol->string key) (string-append path "." (symbol->string key))))

(define (need c h key path)
  (unless (hash? h) (fail c (if (string=? path "") "(body)" path)))
  (hash-ref h key (lambda () (fail c (join path key)))))

(define (need-real c h key path)
  (define v (need c h key path))
  (unless (real? v) (fail c (join path key)))
  v)

(define (need-string c h key path)
  (define v (need c h key path))
  (unless (string? v) (fail c (join path key)))
  v)

(define (need-hash c h key path)
  (define v (need c h key path))
  (unless (hash? v) (fail c (join path key)))
  v)

;; symbol-keyed jsexpr object -> hash string -> real
(define (prob-hash c h path)
  (for/hash ([(k v) (in-hash h)])
    (unless (real? v) (fail c (string-append path "." (symbol->string k))))
    (values (symbol->string k) v)))

;; symbol-keyed jsexpr object with numeric-string keys -> hash integer -> X
(define (level-hash c h path check)
  (for/hash ([(k v) (in-hash h)])
    (define n (string->number (symbol->string k)))
    (unless (exact-nonnegative-integer? n) (fail c (string-append path "." (symbol->string k))))
    (unless (check v) (fail c (string-append path "." (symbol->string k))))
    (values n v)))

(define (parse-answer c id a)
  (define path (string-append "answers." id))
  (define t (need-string c a 'type path))
  (case t
    [("noul")
     (noul-answer 'noul (need-real c a 'noul path))]
    [("choice")
     (define chosen (need-string c a 'choice path))
     (define probs (prob-hash c (need-hash c a 'probabilities path)
                              (string-append path ".probabilities")))
     ;; The winner must appear in its own distribution. If it does not, one of the
     ;; two fields is wrong and we cannot tell which — so neither is trustworthy.
     (unless (hash-has-key? probs chosen) (fail c (string-append path ".choice")))
     (choice-answer 'choice chosen probs (need-real c a 'confidence path))]
    [("score")
     (define legend (level-hash c (need-hash c a 'legend path)
                                (string-append path ".legend") string?))
     (define probs (level-hash c (need-hash c a 'probabilities path)
                               (string-append path ".probabilities") real?))
     ;; Every level that has a probability must have a description, or the number
     ;; the caller reads cannot be explained back to a human.
     (for ([k (in-hash-keys probs)])
       (unless (hash-has-key? legend k)
         (fail c (format "~a.legend.~a" path k))))
     (score-answer 'score (need-real c a 'score path) legend probs
                   (need-real c a 'confidence path))]
    [else (fail c (string-append path ".type"))]))

;; jsexpr -> response
(define (parse-response body #:status [status 200] #:headers [headers '()] #:endpoint [endpoint "POST /v1/systemone"])
  (define c (make-ctx status body headers endpoint))
  (unless (hash? body) (fail c "(body)"))
  (define model (need-string c body 'model ""))
  (define answers (need-hash c body 'answers ""))
  (define u (need-hash c body 'usage ""))
  (response model
            (for/hash ([(k v) (in-hash answers)])
              (define id (symbol->string k))
              (values id (parse-answer c id v)))
            (usage (need-real c u 'input_tokens "usage")
                   (need-real c u 'output_tokens "usage"))))
