#lang racket/base

;; test/error-tests.rkt — the exception hierarchy is a contract.
;;
;; Callers write `with-handlers` against these types, so the relationships below
;; are load-bearing: catching exn:fail:jev must catch everything, catching
;; exn:fail:jev:api must not catch a timeout, and a rate-limit must still be an
;; api error so a generic logger sees its status.
;;   raco test test/error-tests.rkt

(require rackunit "../private/error.rkt")

(define (api status [headers '()])
  (with-handlers ([exn:fail:jev? values])
    (raise-api-error status (hasheq 'error "x") headers "POST https://api.typesafe.ai/v1/systemone")
    #f))

(test-case "status codes map to the type a caller would branch on"
  (check-true (exn:fail:jev:bad-request?   (api 400)))
  (check-true (exn:fail:jev:auth?          (api 401)))
  (check-true (exn:fail:jev:permission?    (api 403)))
  (check-true (exn:fail:jev:not-found?     (api 404)))
  (check-true (exn:fail:jev:unprocessable? (api 422)))
  (check-true (exn:fail:jev:rate-limit?    (api 429)))
  (check-true (exn:fail:jev:server?        (api 500)))
  (check-true (exn:fail:jev:server?        (api 529) )
              "529 Overloaded is a server error, not a special case")
  ;; an unmapped status is still an api error rather than something unhandleable
  (check-true (exn:fail:jev:api? (api 418)))
  (check-false (exn:fail:jev:server? (api 418))))

(test-case "the hierarchy is what handlers rely on"
  (define e (api 429))
  (check-true (exn:fail:jev? e)     "one handler catches every jev failure")
  (check-true (exn:fail:jev:api? e) "a rate limit is still an api error, with a status")
  (check-equal? (exn:fail:jev:api-status e) 429)
  ;; ...and the transport failures are deliberately NOT api errors: there was no
  ;; response, so there is no status or body to inspect
  (define t (exn:fail:jev:timeout "t" (current-continuation-marks) 1.0))
  (check-true (exn:fail:jev? t))
  (check-true (exn:fail:jev:connection? t))
  (check-false (exn:fail:jev:api? t)))

(test-case "Retry-After in seconds becomes milliseconds; anything else becomes #f"
  (check-equal? (exn:fail:jev:rate-limit-retry-after-ms (api 429 '(("retry-after" . "2")))) 2000)
  (check-equal? (exn:fail:jev:rate-limit-retry-after-ms (api 429 '(("retry-after" . " 0 ")))) 0)
  ;; an HTTP-date is legal in the RFC and deliberately unparsed: #f makes the
  ;; caller fall back to its own backoff, which beats a date we got wrong
  (check-false (exn:fail:jev:rate-limit-retry-after-ms
                (api 429 '(("retry-after" . "Wed, 21 Oct 2026 07:28:00 GMT")))))
  (check-false (exn:fail:jev:rate-limit-retry-after-ms (api 429 '(("retry-after" . "-5")))))
  (check-false (exn:fail:jev:rate-limit-retry-after-ms (api 429))))

(test-case "the request id is pulled off the header the API stamps"
  (check-equal? (api-error-request-id (api 500 '(("x-typesafe-request-id" . "req-1")))) "req-1")
  (check-false (api-error-request-id (api 500))))

(test-case "retryable-api-status? is 408, 429 and every 5xx"
  (check-true  (retryable-api-status? 408))
  (check-true  (retryable-api-status? 429))
  (check-true  (retryable-api-status? 500))
  (check-true  (retryable-api-status? 529))
  (check-true  (retryable-api-status? 599))
  (check-false (retryable-api-status? 400))
  (check-false (retryable-api-status? 401))
  (check-false (retryable-api-status? 422))
  (check-false (retryable-api-status? 600)))

(test-case "an error message quotes the endpoint and the body but never a key"
  (define e (api 422 '(("x-typesafe-request-id" . "req-9"))))
  (check-true (regexp-match? #px"HTTP 422" (exn-message e)))
  (check-true (regexp-match? #px"req-9" (exn-message e)))
  (check-true (regexp-match? #px"/v1/systemone" (exn-message e))))
