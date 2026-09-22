#lang racket/base

;; jev — a Racket client for the TypeSafe "System One" API (the Jev model).
;;
;; Built from the published HTTP reference at https://docs.typesafe.ai/api, NOT
;; translated from the vendor's Python SDK. The wire protocol is two endpoints and
;; a handful of JSON shapes; the SDK's bulk is ergonomics that do not survive a
;; move to Racket anyway. Building from the spec keeps the provenance clean and
;; leaves us free of an SDK that is still pre-1.0.
;;
;;   (define c (make-client))                       ; reads TYPESAFE_API_KEY
;;   (define r (ask c "Payouts have failed for 3 days."
;;                  (hash "urgent" (noul "The message conveys time pressure.")
;;                        "team"   (choice "Which team should handle this"
;;                                         (hash "billing"   "Payments and refunds"
;;                                               "technical" "Bugs and integrations"))
;;                        "anger"  (score "How angry the writer sounds"
;;                                        '("Calm" "Annoyed" "Furious")))))
;;   (noul-answer-value (response-ref r "urgent"))  ; 0.95
;;   (answer-confidence (response-ref r "team"))    ; 0.81
;;
;; `choice` and `score` are common words. If they collide in your program, this
;; module is meant to be prefixed:  (require (prefix-in jev: jev))
;;
;; What this client does NOT do, on purpose: no global default client, no
;; implicit environment magic beyond the three documented variables, and no
;; silent coercion of a malformed response into a half-built answer.

(require "private/question.rkt"
         "private/answer.rkt"
         "private/client.rkt"
         "private/http.rkt"
         "private/error.rkt")

(provide
 ;; ---- asking ------------------------------------------------------------------
 make-client (struct-out jev-client)
 ask ask/raw list-models (struct-out model-card)

 ;; ---- questions ---------------------------------------------------------------
 noul choice score
 (struct-out question)
 question->jsexpr questions->jsexpr
 MAX-CHOICE-OPTIONS MIN-SCORE-LEVELS MAX-SCORE-LEVELS

 ;; ---- answers -----------------------------------------------------------------
 (struct-out answer)
 (struct-out noul-answer)
 (struct-out choice-answer)
 (struct-out score-answer)
 (struct-out usage)
 (struct-out response)
 answer-value answer-confidence answer-probabilities
 response-ref response-ids parse-response

 ;; ---- retries and transport ---------------------------------------------------
 (struct-out retry-policy) default-retry-policy retry-status?
 DEFAULT-TIMEOUT DEFAULT-DEADLINE USER-AGENT

 ;; ---- configuration -----------------------------------------------------------
 API-KEY-ENV BASE-URL-ENV DEFAULT-MODEL-ENV DEFAULT-BASE-URL DEFAULT-MODEL

 ;; ---- errors ------------------------------------------------------------------
 (struct-out exn:fail:jev)
 (struct-out exn:fail:jev:api)
 (struct-out exn:fail:jev:bad-request)
 (struct-out exn:fail:jev:auth)
 (struct-out exn:fail:jev:permission)
 (struct-out exn:fail:jev:not-found)
 (struct-out exn:fail:jev:unprocessable)
 (struct-out exn:fail:jev:rate-limit)
 (struct-out exn:fail:jev:server)
 (struct-out exn:fail:jev:connection)
 (struct-out exn:fail:jev:timeout)
 (struct-out exn:fail:jev:response)
 api-error-request-id retryable-api-status?)
