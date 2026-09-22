#lang racket/base

;; test/question-tests.rkt — the three constructors and their validation.
;;
;; The serialization cases are pinned to the EXAMPLE REQUESTS in the published API
;; reference, not to what this implementation happens to emit. If the vendor
;; changes a field name these fail, which is the point.
;;   raco test test/question-tests.rkt

(require rackunit racket/string json "../private/question.rkt")

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

(test-case "score: 2 to 10 levels, per the published range"
  (define (levels n) (for/list ([i (in-range n)]) (format "level ~a" i)))
  (check-not-exn (lambda () (score "q" (levels MIN-SCORE-LEVELS))))
  (check-not-exn (lambda () (score "q" (levels MAX-SCORE-LEVELS))))
  (check-exn #px"at least 2 levels" (lambda () (score "q" (levels 1))))
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
  (check-exn #px"string, object or array" (lambda () (noul 42)))
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
  (check-exn #px"noul, choice or score" (lambda () (questions->jsexpr (hash "a" "not a question"))))
  (check-exn #px"duplicate question id"
             (lambda () (questions->jsexpr (list (cons "a" (noul "q")) (cons "a" (noul "r")))))))
