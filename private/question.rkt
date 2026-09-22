#lang racket/base

;; private/question.rkt — the three question types, validated at CONSTRUCTION.
;;
;; Every limit here is the published API's, not an invention: a Choice takes at
;; most 255 options, a Score takes 2 to 10 ordered levels, and `instructions` may
;; be a string, an object or an array. The API enforces all of this itself and
;; answers 422. We enforce it earlier because a 422 costs a round trip, arrives
;; without the Racket source location that caused it, and — for anything built in
;; a loop — arrives once per item. Reaching a 422 in practice means a hole in
;; THIS file; fix it here rather than teaching callers to read their error body.
;;
;; The constructors are deliberately plain procedures rather than a macro. A
;; question is data, its shape is the vendor's, and a macro would only buy us a
;; nicer literal syntax in exchange for hiding that.

(require racket/list racket/string json)

(provide (struct-out question)
         noul choice score
         question->jsexpr questions->jsexpr
         MAX-CHOICE-OPTIONS MIN-SCORE-LEVELS MAX-SCORE-LEVELS)

;; type         : 'noul | 'choice | 'score
;; instructions : jsexpr (string, object or array)
;; criteria     : jsexpr or #f
(struct question (type instructions criteria) #:transparent)

(define MAX-CHOICE-OPTIONS 255)
(define MIN-SCORE-LEVELS 2)
(define MAX-SCORE-LEVELS 10)

;; ---- shared validation --------------------------------------------------------

;; `instructions` is the one field all three share. A bare string is the common
;; case; an object or array is how you hand the question reference data without
;; smuggling it into `state` (the docs call this structured instructions).
(define (check-instructions who v)
  (cond
    [(string? v)
     (when (string=? (string-trim v) "")
       (error who "instructions must not be blank"))
     v]
    [(or (hash? v) (list? v))
     (unless (jsexpr? v)
       (error who "structured instructions must be JSON-encodable, got ~e" v))
     (when (and (hash? v) (zero? (hash-count v)))
       (error who "structured instructions must not be an empty object"))
     (when (and (list? v) (null? v))
       (error who "structured instructions must not be an empty array"))
     v]
    [else
     (error who "instructions must be a string, object or array, got ~e" v)]))

;; A rubric description: a string, or nothing at all. The API spells "nothing"
;; as JSON null, so #f and 'null both mean "this option needs no extra detail".
(define (check-description who label v)
  (cond [(or (eq? v #f) (eq? v 'null)) 'null]
        [(string? v) v]
        [else (error who "the description for ~s must be a string or #f, got ~e" label v)]))

;; Accept either a hash or an association list. An assoc list is worth supporting
;; because it keeps the options in the order the author wrote them, which makes
;; the source readable; JSON objects are unordered so the wire does not care.
(define (pairs-of who v)
  (cond [(hash? v) (hash-map v cons)]
        [(and (list? v) (andmap pair? v) (not (andmap list? v))) v]
        [(and (list? v) (andmap (lambda (p) (and (list? p) (= 2 (length p)))) v))
         (map (lambda (p) (cons (car p) (cadr p))) v)]
        [else (error who "criteria must be a hash or a list of (option . description) pairs, got ~e" v)]))

;; ---- noul ---------------------------------------------------------------------
;; A yes/no question. The answer is the PROBABILITY of yes, not a boolean — see
;; answer.rkt. #:yes and #:no are optional gloss on what each end means; the API
;; carries them as the object {"true": …, "false": …}, which is why they are
;; keyword arguments here rather than a raw hash the caller has to spell.
(define (noul instructions #:yes [yes #f] #:no [no #f])
  (define who 'noul)
  (define i (check-instructions who instructions))
  (define c
    (and (or yes no)
         (let ([y (check-description who "#:yes" (or yes 'null))]
               [n (check-description who "#:no"  (or no  'null))])
           (hasheq 'true y 'false n))))
  (question 'noul i c))

;; ---- choice -------------------------------------------------------------------
;; Pick one option from a closed set. The answer names the winner AND gives every
;; option a probability, which is the part worth having: a 0.51/0.49 split and a
;; 0.99/0.01 split are the same `choice` and very different decisions.
(define (choice instructions criteria)
  (define who 'choice)
  (define i (check-instructions who instructions))
  (define ps (pairs-of who criteria))
  (when (null? ps)
    (error who "a choice needs at least one option"))
  (when (> (length ps) MAX-CHOICE-OPTIONS)
    (error who "a choice takes at most ~a options, got ~a" MAX-CHOICE-OPTIONS (length ps)))
  (for ([p (in-list ps)])
    (unless (string? (car p))
      (error who "option names must be strings, got ~e" (car p)))
    (when (string=? (car p) "")
      (error who "option names must not be blank")))
  (define names (map car ps))
  (let ([dupes (check-duplicates names)])
    (when dupes (error who "duplicate option ~s" dupes)))
  (question 'choice i
            (for/hasheq ([p (in-list ps)])
              (values (string->symbol (car p)) (check-description who (car p) (cdr p))))))

;; ---- score --------------------------------------------------------------------
;; Rate against ORDERED levels. The answer is probability-weighted and may land
;; between two levels, so a score of 1.05 is a real answer and not a rounding bug.
;; Levels are an array because their order IS the scale; that is the whole
;; difference between a Score and a Choice, and it is why this takes a list.
(define (score instructions criteria)
  (define who 'score)
  (define i (check-instructions who instructions))
  (unless (and (list? criteria) (andmap string? criteria))
    (error who "criteria must be a list of level descriptions, lowest first, got ~e" criteria))
  (define n (length criteria))
  (when (< n MIN-SCORE-LEVELS)
    (error who "a score needs at least ~a levels, got ~a" MIN-SCORE-LEVELS n))
  (when (> n MAX-SCORE-LEVELS)
    (error who "a score takes at most ~a levels, got ~a" MAX-SCORE-LEVELS n))
  (for ([lvl (in-list criteria)])
    (when (string=? (string-trim lvl) "")
      (error who "level descriptions must not be blank")))
  (question 'score i criteria))

;; ---- serialization ------------------------------------------------------------

(define (question->jsexpr q)
  (define base (hasheq 'type (symbol->string (question-type q))
                       'instructions (question-instructions q)))
  (if (question-criteria q)
      (hash-set base 'criteria (question-criteria q))
      base))

;; The ids are the caller's own keys and come back on the answers unchanged. They
;; are never shown to the model, so they are for your code, not for the prompt.
(define (questions->jsexpr qs)
  (define ps
    (cond [(hash? qs) (hash-map qs cons)]
          [(and (list? qs) (andmap pair? qs)) qs]
          [else (error 'ask "questions must be a hash or a list of (id . question) pairs, got ~e" qs)]))
  (when (null? ps) (error 'ask "ask needs at least one question"))
  (for ([p (in-list ps)])
    (unless (string? (car p))
      (error 'ask "question ids must be strings, got ~e" (car p)))
    (when (string=? (car p) "")
      (error 'ask "question ids must not be blank"))
    (unless (question? (cdr p))
      (error 'ask "the value for question id ~s must come from noul, choice or score, got ~e"
             (car p) (cdr p))))
  (let ([dupes (check-duplicates (map car ps))])
    (when dupes (error 'ask "duplicate question id ~s" dupes)))
  (for/hasheq ([p (in-list ps)])
    (values (string->symbol (car p)) (question->jsexpr (cdr p)))))
