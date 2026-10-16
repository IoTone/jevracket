#lang racket/base

;; private/question.rkt — the four question types, validated at CONSTRUCTION.
;;
;; Every limit here is the published API's, not an invention: a Choice takes at
;; most 255 options, a Score takes up to 10 ordered levels, and `instructions`
;; may be a string, an object, an array or an image. The API enforces all of
;; this itself and answers 422. We enforce it earlier because a 422 costs a
;; round trip, arrives without the Racket source location that caused it, and —
;; for anything built in a loop — arrives once per item. Reaching a 422 in
;; practice means a hole in THIS file; fix it here rather than teaching callers
;; to read their error body.
;;
;; `instructions` became OPTIONAL across the API (SDK 0.7.x): a noul may be
;; carried entirely by a described outcome, and a choice or score by its
;; criteria. A noul still needs SOMETHING — instructions or at least one
;; described outcome — or there is no question at all, and that one rule is
;; enforced here as the vendor SDK enforces it.
;;
;; Criteria order is MEANING. A choice's options and the questions map are JSON
;; objects whose key order the model sees, so criteria are STORED as the ordered
;; pairs the author wrote and serialized in that order by jsonw.rkt's writer.
;; `question->jsexpr` still answers an (unordered) jsexpr for callers that want
;; to inspect a question as data; the wire goes through `question->jobj`.
;;
;; The constructors are deliberately plain procedures rather than a macro. A
;; question is data, its shape is the vendor's, and a macro would only buy us a
;; nicer literal syntax in exchange for hiding that.

(require racket/list racket/string json "asset.rkt" "jsonw.rkt")

(provide (struct-out question)
         noul choice score locate
         question->jsexpr questions->jsexpr
         question->jobj questions->jobj
         MAX-CHOICE-OPTIONS MIN-SCORE-LEVELS MAX-SCORE-LEVELS)

;; type         : 'noul | 'choice | 'score | 'box
;; instructions : content (string, object, array or image), or #f for none
;; criteria     : #f, an ordered jobj (noul, choice) or a list (score)
(struct question (type instructions criteria) #:transparent)

(define MAX-CHOICE-OPTIONS 255)
;; The reference says a score "should have at least two levels" — two is where a
;; score means anything — but the API, and the vendor SDK since 0.6.0, accept
;; one. The floor here matches what the API accepts; the advice stays advice.
(define MIN-SCORE-LEVELS 1)
(define MAX-SCORE-LEVELS 10)

;; ---- shared validation --------------------------------------------------------

;; A content value: a string, an object, an array or an image, nested freely.
;; This is `instructions`, and since the 0.6/0.7 API revisions it is also every
;; criteria description and score level.
(define (check-content who what v #:blankable? [blankable? #f])
  (cond
    [(string? v)
     (when (and (not blankable?) (string=? (string-trim v) ""))
       (error who "~a must not be blank" what))
     v]
    [(image? v) v]
    [(or (hash? v) (list? v) (jobj? v))
     (unless (content-ok? v)
       (error who "structured ~a must be JSON-encodable (strings, numbers, objects, arrays, images), got ~e" what v))
     (when (and (hash? v) (zero? (hash-count v)))
       (error who "structured ~a must not be an empty object" what))
     (when (and (list? v) (null? v))
       (error who "structured ~a must not be an empty array" what))
     v]
    [else
     (error who "~a must be a string, object, array or image, got ~e" what v)]))

;; instructions, now optional everywhere: #f means "the criteria carry it".
(define (check-instructions who v)
  (and v (check-content who "instructions" v)))

;; A rubric description: content, or nothing at all. The API spells "nothing"
;; as JSON null, so #f and 'null both mean "this option needs no extra detail".
(define (check-description who label v)
  (cond [(or (eq? v #f) (eq? v 'null)) 'null]
        [else (check-content who (format "the description for ~s" label) v #:blankable? #t)]))

;; Accept either a hash or an association list. An assoc list keeps the options
;; in the order the author wrote them — and that order is part of the question
;; (the wire writer preserves it), so for anything order-sensitive prefer it; a
;; hash is still accepted for compatibility and serializes in a stable order.
(define (pairs-of who v)
  (cond [(hash? v) (hash-map v cons #t)]
        [(and (list? v) (andmap pair? v) (not (andmap list? v))) v]
        [(and (list? v) (andmap (lambda (p) (and (list? p) (= 2 (length p)))) v))
         (map (lambda (p) (cons (car p) (cadr p))) v)]
        [else (error who "criteria must be a hash or a list of (option . description) pairs, got ~e" v)]))

;; ---- noul ---------------------------------------------------------------------
;; A yes/no question. The answer is the PROBABILITY of yes, not a boolean — see
;; answer.rkt. #:yes and #:no are optional gloss on what each end means; the API
;; carries them as the object {"true": …, "false": …}, which is why they are
;; keyword arguments here rather than a raw hash the caller has to spell.
;; Instructions may be omitted when at least one outcome is described:
;; (noul #f #:yes "the invoice is overdue") is a complete question.
(define (noul [instructions #f] #:yes [yes #f] #:no [no #f])
  (define who 'noul)
  (define i (check-instructions who instructions))
  (unless (or i yes no)
    (error who "a noul needs instructions or at least one described outcome (#:yes / #:no)"))
  (define c
    (and (or yes no)
         (let ([y (check-description who "#:yes" (or yes 'null))]
               [n (check-description who "#:no"  (or no  'null))])
           (jobj (list (cons "true" y) (cons "false" n))))))
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
            (jobj (for/list ([p (in-list ps)])
                    (cons (car p) (check-description who (car p) (cdr p)))))))

;; ---- score --------------------------------------------------------------------
;; Rate against ORDERED levels. The answer is probability-weighted and may land
;; between two levels, so a score of 1.05 is a real answer and not a rounding bug.
;; Levels are an array because their order IS the scale; that is the whole
;; difference between a Score and a Choice, and it is why this takes a list.
(define (score instructions criteria)
  (define who 'score)
  (define i (check-instructions who instructions))
  (unless (list? criteria)
    (error who "criteria must be a list of level descriptions, lowest first, got ~e" criteria))
  (define n (length criteria))
  (when (< n MIN-SCORE-LEVELS)
    (error who "a score needs at least ~a level, got ~a" MIN-SCORE-LEVELS n))
  (when (> n MAX-SCORE-LEVELS)
    (error who "a score takes at most ~a levels, got ~a" MAX-SCORE-LEVELS n))
  (question 'score i
            (for/list ([lvl (in-list criteria)] [k (in-naturals)])
              (check-content who (format "level ~a" k) lvl))))

;; ---- locate (the Box primitive) -------------------------------------------------
;; Find what the instructions describe in the request's image; the answer is a
;; box-answer with edges as fractions of the image. The vendor calls this type
;; "box", and so does the wire — the constructor is `locate` only because `box`
;; is one of Racket's most basic names and shadowing it would break any module
;; that uses boxes and this library together.
;;
;; A box question takes no criteria, needs a model that can see (jev-1.14.0 or
;; jev-preview at the time of writing), and an image request needs the opt-in
;; header — see asset.rkt's note.
(define (locate [instructions #f])
  (question 'box (check-instructions 'locate instructions) #f))

;; ---- serialization ------------------------------------------------------------

;; the ordered wire form — what actually goes to the API
(define (question->jobj q)
  (define tail
    (let ([c (question-criteria q)])
      (if c (list (cons "criteria" c)) '())))
  (jobj (append (list (cons "type" (symbol->string (question-type q))))
                (if (question-instructions q)
                    (list (cons "instructions" (question-instructions q)))
                    '())
                tail)))

;; an UNORDERED jsexpr view, for inspecting a question as data and for tests;
;; images make it non-jsexpr, so it is a view, not the wire
(define (question->jsexpr q)
  (define base (hasheq 'type (symbol->string (question-type q))))
  (define with-i (if (question-instructions q)
                     (hash-set base 'instructions (question-instructions q))
                     base))
  (define c (question-criteria q))
  (cond [(not c) with-i]
        [(jobj? c)
         (hash-set with-i 'criteria
                   (for/hasheq ([p (in-list (jobj-pairs c))]) (values (string->symbol (car p)) (cdr p))))]
        [else (hash-set with-i 'criteria c)]))

;; The ids are the caller's own keys and come back on the answers unchanged. They
;; are never shown to the model, so they are for your code, not for the prompt —
;; but their ORDER is shown to the model, so a list of (id . question) pairs
;; serializes in exactly that order.
(define (question-pairs qs)
  (define ps
    (cond [(hash? qs) (hash-map qs cons #t)]
          [(and (list? qs) (andmap pair? qs)) qs]
          [else (error 'ask "questions must be a hash or a list of (id . question) pairs, got ~e" qs)]))
  (when (null? ps) (error 'ask "ask needs at least one question"))
  (for ([p (in-list ps)])
    (unless (string? (car p))
      (error 'ask "question ids must be strings, got ~e" (car p)))
    (when (string=? (car p) "")
      (error 'ask "question ids must not be blank"))
    (unless (question? (cdr p))
      (error 'ask "the value for question id ~s must come from noul, choice, score or locate, got ~e"
             (car p) (cdr p))))
  (let ([dupes (check-duplicates (map car ps))])
    (when dupes (error 'ask "duplicate question id ~s" dupes)))
  ps)

(define (questions->jobj qs)
  (jobj (for/list ([p (in-list (question-pairs qs))])
          (cons (car p) (question->jobj (cdr p))))))

(define (questions->jsexpr qs)
  (for/hasheq ([p (in-list (question-pairs qs))])
    (values (string->symbol (car p)) (question->jsexpr (cdr p)))))
