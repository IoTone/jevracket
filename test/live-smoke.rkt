#lang racket/base

;; test/live-smoke.rkt — the one test that spends money.
;;
;; NOT part of `raco test` (info.rkt omits it). Run it by hand when the vendor
;; ships a model or you change the transport:
;;
;;   TYPESAFE_API_KEY=... racket test/live-smoke.rkt
;;   racket test/live-smoke.rkt            # falls back to ~/.typesafe-key
;;
;; It asks the three primitives in ONE request, which is also the cheapest way to
;; ask them: Jev reads the state once and evaluates every question against it.
;; Exits non-zero on failure so it can gate a release.

(require racket/file racket/string racket/port "../main.rkt")

(define key
  (or (getenv "TYPESAFE_API_KEY")
      (let ([p (build-path (find-system-path 'home-dir) ".typesafe-key")])
        (and (file-exists? p) (string-trim (file->string p))))))

(unless key
  (eprintf "live-smoke: no key. Set TYPESAFE_API_KEY or write ~~/.typesafe-key\n")
  (exit 2))

(define c (make-client #:api-key key))

(define TICKET
  (string-append
   "Hi, I've been trying to connect my Stripe account for 3 days and the "
   "integration keeps failing. I'm losing sales. Please help ASAP."))

(define failures (box 0))
(define (ok! label pass? [detail ""])
  (printf "  ~a  ~a~a\n" (if pass? "ok  " "FAIL") label (if (string=? detail "") "" (format " — ~a" detail)))
  (unless pass? (set-box! failures (add1 (unbox failures)))))

(printf "live-smoke: POST ~a/v1/systemone\n" DEFAULT-BASE-URL)

(define t0 (current-inexact-milliseconds))
(define r
  (ask c TICKET
       (hash "department" (choice "Which team should handle this"
                                  (hash "billing"   "Payment or subscription issues"
                                        "technical" "Bugs or integration problems"
                                        "sales"     "Pricing or account questions"))
             "frustration" (score "How frustrated the customer appears"
                                  '("Calm, just stating facts"
                                    "Frustrated but civil"
                                    "Very angry, strong language"))
             "is_urgent" (noul "The message conveys urgency or time-sensitivity"))))
(define ms (- (current-inexact-milliseconds) t0))

(printf "  model=~a  ~a in / ~a out  ~ams\n"
        (response-model r)
        (usage-input-tokens (response-usage r))
        (usage-output-tokens (response-usage r))
        (inexact->exact (round ms)))

(define dep (response-ref r "department"))
(define fr (response-ref r "frustration"))
(define urg (response-ref r "is_urgent"))

(ok! "all three questions answered in one request" (= 3 (length (response-ids r))))
(ok! "the response names a VERSIONED model, not the alias we sent"
     (not (string=? (response-model r) "jev-latest")) (response-model r))
(ok! "choice picks the technical team" (equal? (choice-answer-value dep) "technical")
     (format "~a @ ~a" (choice-answer-value dep) (answer-confidence dep)))
(ok! "choice probabilities cover every option we offered"
     (= 3 (hash-count (choice-answer-probabilities dep))))
(ok! "score lands inside the levels we defined"
     (and (>= (score-answer-value fr) 0.0) (<= (score-answer-value fr) 2.0))
     (number->string (score-answer-value fr)))
(ok! "score legend came back as integer levels"
     (equal? (sort (hash-keys (score-answer-legend fr)) <) '(0 1 2)))
(ok! "noul reads the urgency" (> (noul-answer-value urg) 0.5)
     (number->string (noul-answer-value urg)))
(ok! "a noul has no confidence, by design" (not (answer-confidence urg)))

;; the other endpoint
(define ms2 (list-models c))
(ok! "list-models returns at least the default alias"
     (and (pair? ms2) (member DEFAULT-MODEL (map model-card-name ms2)) #t)
     (string-join (map model-card-name ms2) ", "))

;; and the unhappy path, against the real server
(define bad (make-client #:api-key "not-a-real-key"))
(ok! "a bad key raises exn:fail:jev:auth"
     (with-handlers ([exn:fail:jev:auth? (lambda (_) #t)] [exn:fail:jev? (lambda (_) #f)])
       (ask bad "x" (hash "q" (noul "Is this a test?"))) #f))

(printf "live-smoke: ~a\n" (if (zero? (unbox failures)) "PASS" "FAIL"))
(exit (if (zero? (unbox failures)) 0 1))
