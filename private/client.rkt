#lang racket/base

;; private/client.rkt — the client and the two calls the API has.
;;
;; The whole surface is `ask` (POST /v1/systemone) and `list-models`
;; (GET /v1/models). That is not a simplification: the vendor's own SDKs wrap the
;; same two endpoints, and most of their bulk is the ergonomics that live in
;; question.rkt and answer.rkt here.
;;
;; The client is a value, not a parameter or a singleton, so a host can hold one
;; per tenant with different keys and budgets and never worry about which one a
;; call picked up.

(require racket/string json "error.rkt" "http.rkt" "question.rkt" "answer.rkt")

(provide (struct-out jev-client)
         (struct-out model-card)
         make-client
         ask ask/raw
         list-models
         API-KEY-ENV BASE-URL-ENV DEFAULT-MODEL-ENV
         DEFAULT-BASE-URL DEFAULT-MODEL)

(define API-KEY-ENV      "TYPESAFE_API_KEY")
(define BASE-URL-ENV     "TYPESAFE_BASE_URL")
(define DEFAULT-MODEL-ENV "TYPESAFE_DEFAULT_MODEL")

(define DEFAULT-BASE-URL "https://api.typesafe.ai")
;; An ALIAS, and it moves when the vendor ships. The response reports the
;; versioned id that actually answered — log it. If you have tuned confidence
;; thresholds, pass the versioned id here instead and move on your own schedule.
(define DEFAULT-MODEL "jev-latest")

(struct jev-client (api-key base-url model timeout policy) #:transparent)

(define (env-or name fallback)
  (define v (getenv name))
  (if (and v (not (string=? (string-trim v) ""))) (string-trim v) fallback))

(define (make-client #:api-key [api-key #f]
                     #:base-url [base-url #f]
                     #:model [model #f]
                     #:timeout [timeout DEFAULT-TIMEOUT]
                     #:retry [policy default-retry-policy])
  (define key (or api-key (env-or API-KEY-ENV #f)))
  ;; Refuse now rather than at the first call. A missing key is a deployment
  ;; mistake, and a 401 three layers into a scheduler job names the wrong thing.
  (unless (and (string? key) (not (string=? (string-trim key) "")))
    (error 'make-client
           "no API key: pass #:api-key or set ~a" API-KEY-ENV))
  (jev-client (string-trim key)
              (let ([b (or base-url (env-or BASE-URL-ENV DEFAULT-BASE-URL))])
                (if (string-suffix? b "/") (substring b 0 (sub1 (string-length b))) b))
              (or model (env-or DEFAULT-MODEL-ENV DEFAULT-MODEL))
              timeout policy))

(define (headers-for c)
  (list (string->bytes/utf-8 (string-append "Authorization: Bearer " (jev-client-api-key c)))
        #"Content-Type: application/json"
        #"Accept: application/json"
        (string->bytes/utf-8 (string-append "User-Agent: " USER-AGENT))))

;; ---- ask ----------------------------------------------------------------------

;; state may be a string or any jsexpr. Structured state is the documented way to
;; hand over a record or a chat log; a string is the common case.
(define (check-state state)
  (unless (jsexpr? state)
    (error 'ask "state must be a string or JSON-encodable data, got ~e" state))
  (when (and (string? state) (string=? (string-trim state) ""))
    (error 'ask "state must not be blank"))
  state)

;; -> response
(define (ask c state questions #:model [model #f])
  (define body (ask/raw c state questions #:model model))
  (parse-response body
                  #:endpoint (format "POST ~a/v1/systemone" (jev-client-base-url c))))

;; The jsexpr body, unparsed. For callers that want to log or replay exactly what
;; the API said, and for tests that pin the wire format.
(define (ask/raw c state questions #:model [model #f])
  (define url (string-append (jev-client-base-url c) "/v1/systemone"))
  (define payload (hasheq 'state (check-state state)
                          'model (or model (jev-client-model c))
                          'questions (questions->jsexpr questions)))
  (define-values (_status _hs text)
    (http-request/retry #"POST" url (headers-for c) (jsexpr->bytes payload)
                        #:timeout (jev-client-timeout c)
                        #:policy (jev-client-policy c)
                        #:endpoint (format "POST ~a/v1/systemone" (jev-client-base-url c))))
  (parse-body text url))

(define (parse-body text url)
  (with-handlers ([exn:fail? (lambda (_)
                               (raise-response-error "(body)" 200 text '()
                                                     (format "POST ~a" url)))])
    (string->jsexpr text)))

;; ---- models -------------------------------------------------------------------

(struct model-card (name description release-date) #:transparent)

(define (list-models c)
  (define url (string-append (jev-client-base-url c) "/v1/models"))
  (define-values (_status _hs text)
    (http-request/retry #"GET" url (headers-for c) #f
                        #:timeout (jev-client-timeout c)
                        #:policy (jev-client-policy c)
                        #:endpoint (format "GET ~a" url)))
  (define body (parse-body text url))
  (define ms (and (hash? body) (hash-ref body 'models #f)))
  (unless (list? ms)
    (raise-response-error "models" 200 body '() (format "GET ~a" url)))
  (for/list ([m (in-list ms)])
    (unless (and (hash? m) (string? (hash-ref m 'name #f)))
      (raise-response-error "models.name" 200 body '() (format "GET ~a" url)))
    (model-card (hash-ref m 'name)
                (let ([d (hash-ref m 'description 'null)]) (if (string? d) d #f))
                (let ([d (hash-ref m 'release_date 'null)]) (if (string? d) d #f)))))
