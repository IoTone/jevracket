#lang racket/base

;; private/client.rkt — the client and the two calls the API has.
;;
;; The whole surface is `ask` (POST /v1/systemone) and `list-models`
;; (GET /v1/models). That is not a simplification: the vendor's own SDKs wrap the
;; same two endpoints, and most of their bulk is the ergonomics that live in
;; question.rkt, answer.rkt and asset.rkt here.
;;
;; The client is a value, not a parameter or a singleton, so a host can hold one
;; per tenant with different keys and budgets and never worry about which one a
;; call picked up.
;;
;; The request body is written by jsonw.rkt's ORDERED writer, never
;; jsexpr->string: the questions map and a choice's options are JSON objects
;; whose key order the model sees, and a Racket hash would scramble it.

(require racket/string racket/list json
         "error.rkt" "http.rkt" "question.rkt" "answer.rkt" "asset.rkt" "jsonw.rkt")

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

;; headers   : extra request headers, (listof (cons string string)) — the place
;;             for the image opt-in, TypeSafe-Danger-Zone-Images: true
;; compress? : gzip request bodies of 1 KiB or more (the SDK default; the API
;;             accepts gzip and zstd). #f sends every body identity-encoded.
(struct jev-client (api-key base-url model timeout policy headers compress?) #:transparent)

(define (env-or name fallback)
  (define v (getenv name))
  (if (and v (not (string=? (string-trim v) ""))) (string-trim v) fallback))

(define (make-client #:api-key [api-key #f]
                     #:base-url [base-url #f]
                     #:model [model #f]
                     #:timeout [timeout DEFAULT-TIMEOUT]
                     #:retry [policy default-retry-policy]
                     #:headers [headers '()]
                     #:compress? [compress? #t])
  (define key (or api-key (env-or API-KEY-ENV #f)))
  ;; Refuse now rather than at the first call. A missing key is a deployment
  ;; mistake, and a 401 three layers into a scheduler job names the wrong thing.
  (unless (and (string? key) (not (string=? (string-trim key) "")))
    (error 'make-client
           "no API key: pass #:api-key or set ~a" API-KEY-ENV))
  (for ([h (in-list headers)])
    (unless (and (pair? h) (string? (car h)) (string? (cdr h)))
      (error 'make-client "#:headers takes (listof (cons string string)), got ~e" h)))
  (jev-client (string-trim key)
              (let ([b (or base-url (env-or BASE-URL-ENV DEFAULT-BASE-URL))])
                (if (string-suffix? b "/") (substring b 0 (sub1 (string-length b))) b))
              (or model (env-or DEFAULT-MODEL-ENV DEFAULT-MODEL))
              timeout policy headers compress?))

(define (headers-for c content-type extra)
  (append
   (list (string->bytes/utf-8 (string-append "Authorization: Bearer " (jev-client-api-key c)))
         (string->bytes/utf-8 (string-append "Content-Type: " content-type))
         #"Accept: application/json"
         (string->bytes/utf-8 (string-append "User-Agent: " USER-AGENT)))
   (for/list ([h (in-list (append (jev-client-headers c) extra))])
     (string->bytes/utf-8 (format "~a: ~a" (car h) (cdr h))))))

;; ---- ask ----------------------------------------------------------------------

;; state may be a string, any jsexpr, or structure containing images. Structured
;; state is the documented way to hand over a record or a chat log; a string is
;; the common case.
(define (check-state state)
  (unless (content-ok? state)
    (error 'ask "state must be a string, JSON-encodable data or images, got ~e" state))
  (when (and (string? state) (string=? (string-trim state) ""))
    (error 'ask "state must not be blank"))
  state)

;; #:assets names images so questions can refer to them: the key is the id your
;; instructions use. (listof (cons string image)) keeps your order; a hash works.
(define (asset-pairs assets)
  (define ps
    (cond [(not assets) '()]
          [(hash? assets) (hash-map assets cons #t)]
          [(and (list? assets) (andmap pair? assets)) assets]
          [else (error 'ask "#:assets must be a hash or a list of (id . image) pairs, got ~e" assets)]))
  (for ([p (in-list ps)])
    (unless (and (string? (car p)) (not (string=? (string-trim (car p)) "")))
      (error 'ask "asset ids must be non-empty strings, got ~e" (car p)))
    (unless (image? (cdr p))
      (error 'ask "the value for asset ~s must be an image, got ~e" (car p) (cdr p))))
  ps)

;; -> response
(define (ask c state questions #:model [model #f] #:assets [assets #f] #:headers [headers '()])
  (define body (ask/raw c state questions #:model model #:assets assets #:headers headers))
  (parse-response body
                  #:endpoint (format "POST ~a/v1/systemone" (jev-client-base-url c))))

;; The jsexpr body, unparsed. For callers that want to log or replay exactly what
;; the API said, and for tests that pin the wire format.
(define (ask/raw c state questions #:model [model #f] #:assets [assets #f] #:headers [headers '()])
  (define url (string-append (jev-client-base-url c) "/v1/systemone"))
  (define aps (asset-pairs assets))
  ;; images named in #:assets serialize as their id; any other image serializes
  ;; as its reference and is collected
  (define aliases (make-hasheq))
  (for ([p (in-list aps)]) (hash-set! aliases (cdr p) (car p)))
  (define found (box '()))
  (define body-jobj
    (jobj (list (cons "state" (check-state state))
                (cons "model" (or model (jev-client-model c)))
                (cons "questions" (questions->jobj questions)))))
  (define body-str (content->json body-jobj aliases found))
  ;; What travels beside the JSON: every named asset, then every image that
  ;; serialized as its reference — plus any named image whose REFERENCE appears
  ;; inside request text (someone format-ed it into a string), which one scan of
  ;; the encoded body finds, the vendor SDK's rule.
  (define by-ref (for/hash ([p (in-list aps)]) (values (image-reference (cdr p)) (cdr p))))
  (define in-text
    (for/list ([m (in-list (regexp-match* REFERENCE-RX body-str))]
               #:when (and (hash-ref by-ref m #f)
                           (not (memq (hash-ref by-ref m) (unbox found)))))
      (cons m (hash-ref by-ref m))))
  (define pending      ; (listof (cons wire-name image)), request order
    (remove-duplicates
     (append aps
             (for/list ([i (in-list (reverse (unbox found)))]) (cons (image-reference i) i))
             in-text)
     #:key car))
  (define-values (content-type data)
    (cond
      [(null? pending) (values "application/json" (string->bytes/utf-8 body-str))]
      [else
       ;; the body names each asset's part before the bytes are read, so the
       ;; JSON part can come first in the multipart body
       (define entries
         (jobj (for/list ([p (in-list pending)] [i (in-naturals)])
                 (cons (car p) (jobj (list (cons "type" "image")
                                           (cons "part" (format "asset-~a" i))))))))
       (define with-assets
         (content->json (jobj (append (jobj-pairs body-jobj) (list (cons "assets" entries))))
                        aliases (box '())))
       (define parts
         (cons (list "request" "application/json" (string->bytes/utf-8 with-assets) #f)
               (for/list ([p (in-list pending)] [i (in-naturals)])
                 (define-values (ctype bs) (image-resolve! (cdr p)))
                 (list (format "asset-~a" i) ctype bs (format "asset-~a" i)))))
       (define-values (mt mb) (encode-multipart parts))
       (values mt mb)]))
  ;; Bodies under 1 KiB fit in one TCP segment; compressing them buys nothing.
  (define compress? (and (jev-client-compress? c)
                         (>= (bytes-length data) MIN-COMPRESSED-BODY-LENGTH)))
  (define hs (headers-for c content-type
                          (append (if compress? '(("Content-Encoding" . "gzip")) '())
                                  headers)))
  (define-values (_status _hs text)
    (http-request/retry #"POST" url hs (if compress? (gzip-bytes data) data)
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
    (http-request/retry #"GET" url (headers-for c "application/json" '()) #f
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
