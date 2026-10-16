#lang racket/base

;; test/question-tests.rkt — the three constructors and their validation.
;;
;; The serialization cases are pinned to the EXAMPLE REQUESTS in the published API
;; reference, not to what this implementation happens to emit. If the vendor
;; changes a field name these fail, which is the point.
;;   raco test test/question-tests.rkt

(require rackunit racket/string json "../private/question.rkt" "../private/jsonw.rkt" "../private/asset.rkt")

;; ---- noul ---------------------------------------------------------------------
(test-case "noul: bare instructions serialize as the docs show"
  (check-equal? (question->jsexpr (noul "Does this convey urgency?"))
                (hasheq 'type "noul" 'instructions "Does this convey urgency?")
                "no criteria key at all when none was given"))

(test-case "noul: #:yes/#:no become the true/false criteria object"
  (check-equal? (question->jsexpr (noul "Does this convey urgency?"
                                        #:yes "Explicitly time-sensitive"
                                        #:no "No urgency expressed"))
                (hasheq 'type "noul"
                        'instructions "Does this convey urgency?"
                        'criteria (hasheq 'true "Explicitly time-sensitive"
                                          'false "No urgency expressed")))
  ;; one side only is legal; the other becomes null rather than vanishing, so the
  ;; object always has both keys the API documents
  (check-equal? (hash-ref (question->jsexpr (noul "q" #:yes "yes means this")) 'criteria)
                (hasheq 'true "yes means this" 'false 'null)))

;; ---- choice -------------------------------------------------------------------
(test-case "choice: option map, exactly as the reference example"
  (define q (choice "Which team should handle this?"
                    (hash "billing"   "Payments, invoicing, refunds"
                          "technical" "Bugs, outages, integrations"
                          "sales"     "Pricing, upgrades, new accounts")))
  (check-equal? (question->jsexpr q)
                (hasheq 'type "choice"
                        'instructions "Which team should handle this?"
                        'criteria (hasheq 'billing "Payments, invoicing, refunds"
                                          'technical "Bugs, outages, integrations"
                                          'sales "Pricing, upgrades, new accounts"))))

(test-case "choice: an assoc list keeps the author's order and means the same thing"
  (check-equal? (question->jsexpr (choice "q" '(("a" . "first") ("b" . "second"))))
                (question->jsexpr (choice "q" (hash "a" "first" "b" "second"))))
  ;; a two-element list per option reads well in source too
  (check-equal? (question->jsexpr (choice "q" '(("a" "first") ("b" "second"))))
                (question->jsexpr (choice "q" (hash "a" "first" "b" "second")))))

(test-case "choice: an option needing no rubric is JSON null, not a missing key"
  (check-equal? (hash-ref (question->jsexpr (choice "q" '(("yes" . #f) ("no" . "nope")))) 'criteria)
                (hasheq 'yes 'null 'no "nope")))

(test-case "choice: the published limit of 255 options is enforced here, not at the API"
  (define (opts n) (for/list ([i (in-range n)]) (cons (format "o~a" i) #f)))
  (check-not-exn (lambda () (choice "q" (opts MAX-CHOICE-OPTIONS))))
  (check-exn #px"at most 255 options" (lambda () (choice "q" (opts (add1 MAX-CHOICE-OPTIONS)))))
  (check-exn #px"at least one option" (lambda () (choice "q" '())))
  (check-exn #px"duplicate option" (lambda () (choice "q" '(("a" . "1") ("a" . "2")))))
  (check-exn #px"must not be blank" (lambda () (choice "q" '(("" . "1"))))))

;; ---- score --------------------------------------------------------------------
(test-case "score: an ordered array, lowest level first"
  (check-equal? (question->jsexpr (score "How frustrated is the customer?"
                                         '("Calm" "Frustrated" "Very angry")))
                (hasheq 'type "score"
                        'instructions "How frustrated is the customer?"
                        'criteria '("Calm" "Frustrated" "Very angry"))))

(test-case "score: 1 to 10 levels — the API accepts a single level since 0.6, though two is where a score means anything"
  (define (levels n) (for/list ([i (in-range n)]) (format "level ~a" i)))
  (check-not-exn (lambda () (score "q" (levels MIN-SCORE-LEVELS))))
  (check-not-exn (lambda () (score "q" (levels MAX-SCORE-LEVELS))))
  (check-exn #px"at least 1 level" (lambda () (score "q" (levels 0))))
  (check-exn #px"at most 10 levels" (lambda () (score "q" (levels 11))))
  ;; a hash would lose the ordering that IS the scale
  (check-exn #px"list of level descriptions" (lambda () (score "q" (hash "a" "b")))))

;; ---- instructions -------------------------------------------------------------
(test-case "instructions accept the documented structured form"
  (define structured
    (hasheq 'potential_duplicate (hasheq 'name "John Smith" 'location "Oakland, California")
            'question "Is the resume for the same person as `potential_duplicate`?"))
  (check-equal? (hash-ref (question->jsexpr (noul structured)) 'instructions) structured)
  (check-not-exn (lambda () (noul '("a" "b")))))

(test-case "instructions are refused when blank or unencodable"
  (check-exn #px"must not be blank" (lambda () (noul "   ")))
  (check-exn #px"string, object, array or image" (lambda () (noul 42)))
  (check-exn #px"must not be an empty object" (lambda () (noul (hasheq))))
  (check-exn #px"JSON-encodable" (lambda () (noul (list (lambda (x) x))))))

;; ---- the questions map --------------------------------------------------------
(test-case "questions->jsexpr keys the map by the caller's ids"
  (define m (questions->jsexpr (hash "is_urgent" (noul "Does this convey urgency?"))))
  (check-equal? m (hasheq 'is_urgent (hasheq 'type "noul" 'instructions "Does this convey urgency?")))
  ;; and the whole request body is JSON, which is the only claim that matters
  (check-true (jsexpr? m)))

(test-case "questions->jsexpr refuses what would become a confusing 422"
  (check-exn #px"at least one question" (lambda () (questions->jsexpr (hash))))
  (check-exn #px"must be strings" (lambda () (questions->jsexpr (list (cons 'sym (noul "q"))))))
  (check-exn #px"must not be blank" (lambda () (questions->jsexpr (list (cons "" (noul "q"))))))
  (check-exn #px"noul, choice, score or locate" (lambda () (questions->jsexpr (hash "a" "not a question"))))
  (check-exn #px"duplicate question id"
             (lambda () (questions->jsexpr (list (cons "a" (noul "q")) (cons "a" (noul "r")))))))

;; ---- optional instructions (SDK 0.7.x) ------------------------------------------
(test-case "instructions are optional when the criteria carry the question"
  ;; a noul carried entirely by a described outcome
  (check-equal? (question->jsexpr (noul #:yes "the invoice is past its due date"))
                (hasheq 'type "noul"
                        'criteria (hasheq 'true "the invoice is past its due date" 'false 'null))
                "no instructions key at all when none was given")
  ;; a choice or score may ride on its criteria alone
  (check-not-exn (lambda () (choice #f '(("a" . "first") ("b" . "second")))))
  (check-not-exn (lambda () (score #f '("Calm" "Furious"))))
  (check-false (hash-has-key? (question->jsexpr (choice #f '(("a" . #f)))) 'instructions))
  ;; but a noul with NEITHER has no question in it at all
  (check-exn #px"instructions or at least one described outcome" (lambda () (noul)))
  (check-exn #px"instructions or at least one described outcome" (lambda () (noul #f))))

;; ---- structured descriptions (API 0.6+: criteria take objects and arrays) -------
(test-case "criteria descriptions and score levels accept structured content"
  (define rubric (hasheq 'tone "hostile" 'examples '("x" "y")))
  (check-equal? (hash-ref (hash-ref (question->jsexpr (choice "q" (list (cons "angry" rubric)
                                                                        (cons "calm" "no heat"))))
                                    'criteria)
                          'angry)
                rubric)
  (check-equal? (hash-ref (question->jsexpr (noul "q" #:yes rubric)) 'criteria)
                (hasheq 'true rubric 'false 'null))
  (check-equal? (hash-ref (question->jsexpr (score "q" (list "low" rubric))) 'criteria)
                (list "low" rubric)))

;; ---- locate (the Box primitive) --------------------------------------------------
(test-case "locate serializes as the box type the wire documents"
  (check-equal? (question->jsexpr (locate "the coffee mug in `photo`"))
                (hasheq 'type "box" 'instructions "the coffee mug in `photo`"))
  ;; no criteria, ever; instructions optional like everything else now
  (check-equal? (question->jsexpr (locate)) (hasheq 'type "box"))
  (check-not-exn (lambda () (questions->jsexpr (hash "mug" (locate "the mug"))))))

;; ---- order is part of the question (the wire writer) -----------------------------
(define (pos* s needle) (car (car (regexp-match-positions (regexp (regexp-quote needle)) s))))
(test-case "the wire form keeps the author's option and question order"
  ;; jsexpr cannot carry key order; the jobj writer can, and the model sees it —
  ;; measured on the open-weight checkpoint, reordering options moved a
  ;; probability by 0.54
  (define q (choice "q" '(("zebra" . "z") ("apple" . "a") ("mango" . "m"))))
  (define wire (content->json (question->jobj q) (make-hasheq) (box '())))
  (define (pos s) (car (car (regexp-match-positions (regexp (regexp-quote s)) wire))))
  (check-true (< (pos "zebra") (pos "apple") (pos "mango"))
              "options serialize in the order the author wrote them")
  (define qs (questions->jobj (list (cons "second_look" (noul "b"))
                                    (cons "first_look" (noul "a")))))
  (define qwire (content->json qs (make-hasheq) (box '())))
  (check-true (< (pos* qwire "second_look") (pos* qwire "first_look"))
              "the questions map serializes in the order the caller listed them"))

(test-case "non-finite numbers serialize as the strings the API's SDKs send"
  (check-equal? (content->json (list +inf.0 -inf.0 +nan.0 1.5) (make-hasheq) (box '()))
                "[\"Infinity\",\"-Infinity\",\"NaN\",1.5]"))
