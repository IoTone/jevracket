#lang racket/base

;; test/client-tests.rkt — the client against a real socket.
;;
;; Everything here drives test/mock-api.rkt over TCP rather than stubbing the
;; transport, because the failures worth catching live in the transport: a header
;; the client forgets to send, a Content-Length it never sets, a retry that counts
;; wrong, a 401 that gets retried three times before failing.
;;   raco test test/client-tests.rkt

(require rackunit racket/string racket/list json
         "../main.rkt" "mock-api.rkt")

;; Fast retries: the timing rules are asserted by counting attempts, not by
;; waiting for real backoff.
(define (fast #:max [max-retries 2] #:connection? [conn? #t] #:deadline [deadline 5.0])
  (retry-policy max-retries 0.01 0.02 0.0 retryable-api-status? #t conn? deadline))

(define (with-mock respond proc #:policy [policy (fast)] #:timeout [timeout 2.0])
  (define m (start-mock! respond))
  (dynamic-wind
   void
   (lambda ()
     (proc m (make-client #:api-key "test-key-123"
                          #:base-url (mock-url m)
                          #:timeout timeout
                          #:retry policy)))
   (lambda () (mock-stop! m))))

(define OK-BODY
  "{\"model\":\"jev-1.13.0\",\"answers\":{\"is_urgent\":{\"type\":\"noul\",\"noul\":0.95}},\"usage\":{\"input_tokens\":307,\"output_tokens\":20}}")

(define (ok-always n r) (list 200 '() OK-BODY))

(define QS (hash "is_urgent" (noul "Does this convey urgency?")))

;; ---- the request the client actually puts on the wire --------------------------

(test-case "ask sends the documented request and parses the documented response"
  (with-mock ok-always
    (lambda (m c)
      (define r (ask c "Help! My payouts have been failing for 3 days." QS))
      (check-equal? (response-model r) "jev-1.13.0")
      (check-equal? (noul-answer-value (response-ref r "is_urgent")) 0.95)

      (check-equal? (mock-count m) 1)
      (define req (first (mock-requests m)))
      (check-equal? (recorded-method req) "POST")
      (check-equal? (recorded-path req) "/v1/systemone")
      (define hs (recorded-headers req))
      (check-equal? (cdr (assoc "authorization" hs)) "Bearer test-key-123")
      (check-equal? (cdr (assoc "content-type" hs)) "application/json")
      (check-true (string-prefix? (cdr (assoc "user-agent" hs)) "jev-racket/"))
      ;; a body with no length is a body the server may never finish reading
      (check-true (and (assoc "content-length" hs) #t) "Content-Length is sent")

      (define sent (string->jsexpr (recorded-body req)))
      (check-equal? (hash-ref sent 'state) "Help! My payouts have been failing for 3 days.")
      (check-equal? (hash-ref sent 'model) "jev-latest" "the default model is the documented alias")
      (check-equal? (hash-ref sent 'questions)
                    (hasheq 'is_urgent (hasheq 'type "noul" 'instructions "Does this convey urgency?"))))))

(test-case "#:model overrides the client default per call"
  (with-mock ok-always
    (lambda (m c)
      (ask c "x" QS #:model "jev-1.13.0")
      (check-equal? (hash-ref (string->jsexpr (recorded-body (first (mock-requests m)))) 'model)
                    "jev-1.13.0"))))

(test-case "structured state travels as JSON, not as a printed string"
  (with-mock ok-always
    (lambda (m c)
      (ask c (hasheq 'ticket "late" 'customer (hasheq 'tier "gold")) QS)
      (check-equal? (hash-ref (string->jsexpr (recorded-body (first (mock-requests m)))) 'state)
                    (hasheq 'ticket "late" 'customer (hasheq 'tier "gold"))))))

;; ---- retries -------------------------------------------------------------------

(test-case "429 is retried, and Retry-After is honoured over the backoff"
  (with-mock (lambda (n r)
               (if (= n 0)
                   (list 429 '(("Retry-After" . "0") ("x-typesafe-request-id" . "req-abc")) "{\"error\":\"slow down\"}")
                   (list 200 '() OK-BODY)))
    (lambda (m c)
      (define r (ask c "x" QS))
      (check-equal? (response-model r) "jev-1.13.0")
      (check-equal? (mock-count m) 2 "one retry, then success"))))

(test-case "529 Overloaded is retried — it is a 5xx, which is why it needs no special case"
  (with-mock (lambda (n r) (if (< n 2) (list 529 '() "{\"error\":\"overloaded\"}") (list 200 '() OK-BODY)))
    (lambda (m c)
      (check-equal? (response-model (ask c "x" QS)) "jev-1.13.0")
      (check-equal? (mock-count m) 3))))

(test-case "a persistent 500 is retried exactly max-retries times, then raises"
  (with-mock (lambda (n r) (list 500 '() "{\"error\":\"boom\"}"))
    (lambda (m c)
      (check-exn exn:fail:jev:server? (lambda () (ask c "x" QS)))
      (check-equal? (mock-count m) 3 "the first attempt plus two retries"))))

(test-case "max-retries 0 disables retrying"
  (with-mock (lambda (n r) (list 500 '() "{}"))
    #:policy (fast #:max 0)
    (lambda (m c)
      (check-exn exn:fail:jev:server? (lambda () (ask c "x" QS)))
      (check-equal? (mock-count m) 1))))

;; ---- errors that must NOT be retried -------------------------------------------

(test-case "401 fails immediately: a wrong key will still be wrong in 500ms"
  (with-mock (lambda (n r) (list 401 '(("x-typesafe-request-id" . "req-401")) "{\"error\":\"invalid api key\"}"))
    (lambda (m c)
      (define e (with-handlers ([exn:fail:jev:auth? values]) (ask c "x" QS) #f))
      (check-true (exn:fail:jev:auth? e))
      (check-equal? (exn:fail:jev:api-status e) 401)
      (check-equal? (api-error-request-id e) "req-401" "the request id is quotable in a bug report")
      (check-equal? (mock-count m) 1 "not retried"))))

(test-case "422 carries the server's complaint about which field was wrong"
  (with-mock (lambda (n r) (list 422 '() "{\"error\":{\"field\":\"questions.a.criteria\"}}"))
    (lambda (m c)
      (define e (with-handlers ([exn:fail:jev:unprocessable? values]) (ask c "x" QS) #f))
      (check-true (exn:fail:jev:unprocessable? e))
      (check-equal? (exn:fail:jev:api-body e) (hasheq 'error (hasheq 'field "questions.a.criteria")))
      (check-equal? (mock-count m) 1)
      ;; the endpoint is in the message and carries no key
      (check-true (regexp-match? #px"/v1/systemone" (exn-message e)))
      (check-false (regexp-match? #px"test-key-123" (exn-message e))
                   "the API key never appears in an error message"))))

(test-case "404 and 403 map to their own types"
  (with-mock (lambda (n r) (list 404 '() "{}"))
    (lambda (m c) (check-exn exn:fail:jev:not-found? (lambda () (ask c "x" QS)))))
  (with-mock (lambda (n r) (list 403 '() "{}"))
    (lambda (m c) (check-exn exn:fail:jev:permission? (lambda () (ask c "x" QS))))))

;; ---- broken successes ----------------------------------------------------------

(test-case "a 200 that is not JSON is a response error, not a crash"
  (with-mock (lambda (n r) (list 200 '() "<html>gateway</html>"))
    (lambda (m c) (check-exn exn:fail:jev:response? (lambda () (ask c "x" QS))))))

(test-case "a 200 missing a field names the field"
  (with-mock (lambda (n r) (list 200 '() "{\"model\":\"m\",\"answers\":{\"is_urgent\":{\"type\":\"noul\"}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}"))
    (lambda (m c)
      (define e (with-handlers ([exn:fail:jev:response? values]) (ask c "x" QS) #f))
      (check-equal? (exn:fail:jev:response-field-path e) "answers.is_urgent.noul"))))

;; ---- timeouts ------------------------------------------------------------------

(test-case "a server that never answers times out, and says how long it waited"
  (with-mock #f #:policy (fast #:max 0 #:connection? #f) #:timeout 0.4
    (lambda (m c)
      (define e (with-handlers ([exn:fail:jev:timeout? values]) (ask c "x" QS) #f))
      (check-true (exn:fail:jev:timeout? e))
      (check-equal? (exn:fail:jev:timeout-timeout e) 0.4))))

(test-case "a timeout is retried when the policy allows it"
  ;; first attempt hangs, second answers
  (with-mock (lambda (n r) (if (= n 0) #f (list 200 '() OK-BODY)))
    #:policy (fast #:max 2 #:connection? #t) #:timeout 0.4
    (lambda (m c)
      (check-equal? (response-model (ask c "x" QS)) "jev-1.13.0")
      (check-equal? (mock-count m) 2))))

;; ---- models --------------------------------------------------------------------

(test-case "list-models reads the model catalogue"
  (with-mock (lambda (n r)
               (list 200 '() "{\"models\":[{\"name\":\"jev-latest\",\"description\":\"most recent stable\",\"release_date\":\"2026-08-01\"}]}"))
    (lambda (m c)
      (define ms (list-models c))
      (check-equal? (length ms) 1)
      (check-equal? (model-card-name (first ms)) "jev-latest")
      (check-equal? (model-card-release-date (first ms)) "2026-08-01")
      (check-equal? (recorded-method (first (mock-requests m))) "GET")
      (check-equal? (recorded-path (first (mock-requests m))) "/v1/models"))))

;; ---- construction --------------------------------------------------------------

(test-case "a missing key is refused at construction, not at the first call"
  (check-exn #px"no API key" (lambda () (make-client #:api-key "")))
  (check-exn #px"TYPESAFE_API_KEY" (lambda () (make-client #:api-key "   "))))

(test-case "a trailing slash on the base URL does not produce a doubled path"
  (with-mock ok-always
    (lambda (m c)
      (define c2 (make-client #:api-key "k" #:base-url (string-append (mock-url m) "/") #:retry (fast)))
      (ask c2 "x" QS)
      (check-equal? (recorded-path (first (mock-requests m))) "/v1/systemone"))))

(test-case "blank state is refused before a request is made"
  (with-mock ok-always
    (lambda (m c)
      (check-exn #px"must not be blank" (lambda () (ask c "  " QS)))
      (check-equal? (mock-count m) 0 "nothing was sent"))))

;; ---- the new wire: order, images, compression, headers ---------------------------

(require file/gunzip "../private/asset.rkt")

(define (gunzip-body bs)
  (define out (open-output-bytes))
  (gunzip-through-ports (open-input-bytes bs) out)
  (get-output-bytes out))

(test-case "question and option order reaches the wire as the author wrote it"
  (with-mock ok-always
    (lambda (m c)
      (ask c "x" (list (cons "zz_first" (noul "a"))
                       (cons "aa_second" (choice "q" '(("zebra" . #f) ("apple" . #f))))))
      (define body (recorded-body (first (mock-requests m))))
      (define (pos s) (car (car (regexp-match-positions (regexp (regexp-quote s)) body))))
      (check-true (< (pos "zz_first") (pos "aa_second")))
      (check-true (< (pos "zebra") (pos "apple"))))))

(test-case "a client or per-call header rides every request — the image opt-in's path"
  (with-mock ok-always
    (lambda (m c0)
      (define c (make-client #:api-key "k" #:base-url (mock-url m) #:retry (fast)
                             #:headers '(("TypeSafe-Danger-Zone-Images" . "true"))))
      (ask c "x" QS #:headers '(("X-Case" . "per-call")))
      (define hs (recorded-headers (first (mock-requests m))))
      (check-equal? (cdr (assoc "typesafe-danger-zone-images" hs)) "true")
      (check-equal? (cdr (assoc "x-case" hs)) "per-call"))))

(test-case "an image makes the request multipart: the JSON names the part that carries the bytes"
  (define png (bytes-append #"\211PNG\r\n\32\n" (make-bytes 16 0)))
  (with-mock ok-always
    (lambda (m c)
      (define photo (image png))
      (ask c (hasheq 'photo photo)
           (hash "mug" (locate "the coffee mug in `photo`"))
           #:assets (list (cons "photo" photo)))
      (define req (first (mock-requests m)))
      (define hs (recorded-headers req))
      (check-true (string-prefix? (cdr (assoc "content-type" hs)) "multipart/form-data; boundary=")
                  "the content type switched to multipart")
      (define body (recorded-body-bytes req))
      ;; the JSON part names the asset and its part...
      (define jm (regexp-match #px#"name=\"request\"\r\nContent-Type: application/json\r\n\r\n(.*?)\r\n--" body))
      (check-true (and jm #t) "the first part is the JSON, named request")
      (define sent (string->jsexpr (bytes->string/utf-8 (cadr jm))))
      (check-equal? (hash-ref sent 'assets)
                    (hasheq 'photo (hasheq 'type "image" 'part "asset-0")))
      (check-equal? (hash-ref sent 'state) (hasheq 'photo "photo")
                    "the aliased image serializes as its id")
      ;; ...and the bytes ride in that part, verbatim
      (check-true (regexp-match? #px#"name=\"asset-0\"" body))
      (check-true (regexp-match? #px#"Content-Type: image/png" body))
      (check-true (regexp-match? (byte-regexp png) body)))))

(test-case "an anonymous image is collected under its reference"
  (define jpg (bytes-append #"\377\330\377" (make-bytes 16 0)))
  (with-mock ok-always
    (lambda (m c)
      (define photo (image jpg))
      (ask c (hasheq 'photo photo) (hash "is_cat" (noul "Is `photo` a cat?")))
      (define body (recorded-body-bytes (first (mock-requests m))))
      (define jm (regexp-match #px#"name=\"request\"\r\nContent-Type: application/json\r\n\r\n(.*?)\r\n--" body))
      (define sent (string->jsexpr (bytes->string/utf-8 (cadr jm))))
      (define ref (string->symbol (image-reference photo)))
      (check-equal? (hash-ref sent 'state) (hasheq 'photo (image-reference photo)))
      (check-equal? (hash-ref (hash-ref sent 'assets) ref)
                    (hasheq 'type "image" 'part "asset-0")))))

(test-case "a body of 1 KiB or more goes out gzipped; a small one does not"
  (with-mock ok-always
    (lambda (m c)
      (ask c (make-string 2048 #\x) QS)
      (define req (first (mock-requests m)))
      (check-equal? (cdr (assoc "content-encoding" (recorded-headers req))) "gzip")
      (define sent (string->jsexpr (bytes->string/utf-8 (gunzip-body (recorded-body-bytes req)))))
      (check-equal? (string-length (hash-ref sent 'state)) 2048)
      ;; a small body is identity-encoded — below one TCP segment there is
      ;; nothing to win
      (ask c "small" QS)
      (check-false (assoc "content-encoding" (recorded-headers (second (mock-requests m))))))))

(test-case "#:compress? #f turns compression off however large the body"
  (with-mock ok-always
    (lambda (m c0)
      (define c (make-client #:api-key "k" #:base-url (mock-url m) #:retry (fast) #:compress? #f))
      (ask c (make-string 2048 #\x) QS)
      (check-false (assoc "content-encoding" (recorded-headers (first (mock-requests m))))))))
