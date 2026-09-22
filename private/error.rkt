#lang racket/base

;; private/error.rkt — the exception taxonomy.
;;
;; One base type, `exn:fail:jev`, so a caller that only wants "did the Jev call
;; fail" writes one handler. Below it the split is by what the caller can DO
;; about it, which is not the same as the HTTP status:
;;
;;   exn:fail:jev:api         the server answered, unhappily. Carries the status,
;;                            the parsed body, the headers and the request id, so
;;                            a bug report can name the request.
;;     :bad-request  400      your JSON was not JSON
;;     :auth         401      the key is wrong or missing — never retry
;;     :permission   403      the key is real but not allowed here
;;     :not-found    404
;;     :unprocessable 422     the body failed THEIR validation; the body names the
;;                            offending field. Reaching this means our own
;;                            question validation has a hole — see question.rkt.
;;     :rate-limit   429      carries retry-after-ms when the server sent one
;;     :server       5xx      includes 529 Overloaded, which is Jev's "try later"
;;   exn:fail:jev:connection  no HTTP response at all
;;     :timeout               ...because we gave up first
;;   exn:fail:jev:response    a 2xx whose body was missing or malformed. Separate
;;                            from :api on purpose: the request was fine, the
;;                            CONTRACT was broken, and `field-path` says where.
;;
;; A note on why :response exists at all. The whole point of this API is that you
;; get a typed answer instead of parsing prose. If we let a missing `confidence`
;; slide through as #f, every caller would have to re-check what the type system
;; just promised, and the one thing a type-safe client must never do is hand back
;; a half-built answer. So: raise, and name the field.

(require racket/string)

(provide (struct-out exn:fail:jev)
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
         raise-api-error raise-response-error
         api-error-request-id
         retryable-api-status?)

(struct exn:fail:jev exn:fail () #:transparent)

;; status : exact-integer
;; body   : jsexpr or string or #f   (the server's error body, as it arrived)
;; headers: (listof (cons string string))  lower-cased names
;; endpoint: string, e.g. "POST https://api.typesafe.ai/v1/systemone" — no key,
;;           no query, so it is safe to log verbatim
(struct exn:fail:jev:api exn:fail:jev (status body headers endpoint) #:transparent)

(struct exn:fail:jev:bad-request   exn:fail:jev:api () #:transparent)
(struct exn:fail:jev:auth          exn:fail:jev:api () #:transparent)
(struct exn:fail:jev:permission    exn:fail:jev:api () #:transparent)
(struct exn:fail:jev:not-found     exn:fail:jev:api () #:transparent)
(struct exn:fail:jev:unprocessable exn:fail:jev:api () #:transparent)
;; retry-after-ms : exact-nonnegative-integer or #f
(struct exn:fail:jev:rate-limit    exn:fail:jev:api (retry-after-ms) #:transparent)
(struct exn:fail:jev:server        exn:fail:jev:api () #:transparent)

(struct exn:fail:jev:connection exn:fail:jev () #:transparent)
;; timeout : the seconds we waited
(struct exn:fail:jev:timeout exn:fail:jev:connection (timeout) #:transparent)

;; field-path : "answers.tone.confidence"
(struct exn:fail:jev:response exn:fail:jev:api (field-path) #:transparent)

;; The header the API stamps on every response. Quote it in a bug report and
;; TypeSafe can find the request.
(define (api-error-request-id e)
  (cond [(assoc "x-typesafe-request-id" (exn:fail:jev:api-headers e)) => cdr]
        [else #f]))

;; Is this status worth trying again? 408 and 429 plus the whole 5xx range, which
;; is how 529 Overloaded gets covered without naming it.
(define (retryable-api-status? code)
  (or (= code 408) (= code 429) (and (>= code 500) (< code 600))))

;; A short, non-secret description of what came back, for the exception message.
(define (body-excerpt body)
  (define s (cond [(string? body) body]
                  [(not body) ""]
                  [else (format "~s" body)]))
  (define t (string-trim s))
  (cond [(string=? t "") ""]
        [(> (string-length t) 300) (string-append ": " (substring t 0 300) "…")]
        [else (string-append ": " t)]))

(define (retry-after-ms headers)
  (define v (cond [(assoc "retry-after" headers) => cdr] [else #f]))
  (and v
       (let ([n (string->number (string-trim v))])
         ;; Seconds as an integer is what the API sends. An HTTP-date is legal in
         ;; the RFC and we deliberately do NOT parse it: returning #f makes the
         ;; caller fall back to its own backoff, which is correct-but-slower,
         ;; whereas a date we mis-parsed would be confidently wrong.
         (and n (real? n) (>= n 0) (inexact->exact (round (* n 1000)))))))

(define (raise-api-error status body headers endpoint)
  (define rid (cond [(assoc "x-typesafe-request-id" headers) => cdr] [else #f]))
  (define msg (format "jev: ~a returned HTTP ~a~a~a"
                      endpoint status
                      (if rid (format " (request ~a)" rid) "")
                      (body-excerpt body)))
  (define marks (current-continuation-marks))
  (raise
   (case status
     [(400) (exn:fail:jev:bad-request   msg marks status body headers endpoint)]
     [(401) (exn:fail:jev:auth          msg marks status body headers endpoint)]
     [(403) (exn:fail:jev:permission    msg marks status body headers endpoint)]
     [(404) (exn:fail:jev:not-found     msg marks status body headers endpoint)]
     [(422) (exn:fail:jev:unprocessable msg marks status body headers endpoint)]
     [(429) (exn:fail:jev:rate-limit    msg marks status body headers endpoint
                                        (retry-after-ms headers))]
     [else
      (if (and (>= status 500) (< status 600))
          (exn:fail:jev:server msg marks status body headers endpoint)
          (exn:fail:jev:api    msg marks status body headers endpoint))])))

(define (raise-response-error field-path status body headers endpoint)
  (raise (exn:fail:jev:response
          (format "jev: ~a answered HTTP ~a but the body is missing or malformed at `~a`"
                  endpoint status field-path)
          (current-continuation-marks) status body headers endpoint field-path)))
