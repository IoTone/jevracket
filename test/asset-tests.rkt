#lang racket/base

;; test/asset-tests.rkt — image values, their references, the multipart body and
;; the writer's asset collection.
;;   raco test test/asset-tests.rkt

(require rackunit racket/string racket/file file/gunzip net/base64 json
         "../private/asset.rkt" "../private/jsonw.rkt")

(define (tiny-png) (bytes-append #"\211PNG\r\n\32\n" (make-bytes 16 0)))
(define (tiny-jpeg) (bytes-append #"\377\330\377" (make-bytes 16 0)))

;; bytes -> (listof bytes), split on a byte pattern — spelled here because
;; racket has no regexp-split and three assertions want the parts
(define (split-on rx bs)
  (let loop ([start 0] [acc '()])
    (define m (regexp-match-positions rx bs start))
    (if m
        (loop (cdar m) (cons (subbytes bs start (caar m)) acc))
        (reverse (cons (subbytes bs start) acc)))))

;; ---- the image value --------------------------------------------------------------

(test-case "an image resolves once, and its content type comes from the signature"
  (define i (image (tiny-png)))
  (define-values (ct bs) (image-resolve! i))
  (check-equal? ct "image/png")
  (check-equal? bs (tiny-png))
  (define-values (ct2 _bs2) (image-resolve! (image (tiny-jpeg))))
  (check-equal? ct2 "image/jpeg")
  ;; the API detects formats itself; an unrecognized image still goes, labeled
  ;; as generic binary data
  (define-values (ct3 _bs3) (image-resolve! (image #"GIF89a....")))
  (check-equal? ct3 "application/octet-stream"))

(test-case "a path is read on first use, and a missing file names itself"
  (define p (make-temporary-file))
  (call-with-output-file p #:exists 'replace (lambda (o) (write-bytes (tiny-png) o)))
  (define-values (ct _bs) (image-resolve! (image p)))
  (check-equal? ct "image/png")
  (delete-file p)
  (check-exn #px"could not be read" (lambda () (image-resolve! (image p)))))

(test-case "empty data and non-image values are refused"
  (check-exn #px"is empty" (lambda () (image-resolve! (image #""))))
  (check-exn #px"image file bytes or a path" (lambda () (image 42))))

(test-case "base64 decodes strictly, forgiving only whitespace"
  (define wrapped (bytes->string/utf-8 (base64-encode (tiny-png))))  ; carries \r\n
  (define-values (_ct bs) (image-resolve! (image-from-base64 wrapped)))
  (check-equal? bs (tiny-png))
  (check-exn #px"valid base64" (lambda () (image-from-base64 "n0t/b@se64!")))
  (check-exn #px"non-empty" (lambda () (image-from-base64 "  \n "))))

(test-case "every image gets a distinct scannable reference"
  (define a (image (tiny-png)))
  (define b (image (tiny-png)))
  (check-not-equal? (image-reference a) (image-reference b))
  (check-true (regexp-match? REFERENCE-RX (image-reference a))))

;; ---- the writer and assets ---------------------------------------------------------

(test-case "an aliased image serializes as its alias; an anonymous one as its reference, and is collected"
  (define named (image (tiny-png)))
  (define anon  (image (tiny-jpeg)))
  (define aliases (make-hasheq (list (cons named "photo-1"))))
  (define found (box '()))
  (define out (content->json (list named anon "text") aliases found))
  (check-equal? out (format "[\"photo-1\",~a,\"text\"]"
                            (jsexpr->string (image-reference anon))))
  (check-equal? (unbox found) (list anon) "only the anonymous image needs collecting")
  ;; met twice, collected once
  (define found2 (box '()))
  (content->json (list anon anon) (make-hasheq) found2)
  (check-equal? (unbox found2) (list anon)))

;; ---- multipart ---------------------------------------------------------------------

(test-case "the multipart body carries the JSON part first, then each asset's bytes"
  (define-values (ctype body)
    (encode-multipart (list (list "request" "application/json" #"{\"a\":1}" #f)
                            (list "asset-0" "image/png" (tiny-png) "asset-0"))))
  (define m (regexp-match #px"^multipart/form-data; boundary=([0-9a-f]+)$" ctype))
  (check-true (and m #t) "the content type names the boundary")
  (define boundary (string->bytes/utf-8 (cadr m)))
  (define parts (split-on (byte-regexp (bytes-append #"--" boundary)) body))
  ;; leading empty chunk, two parts, the closing "--\r\n"
  (check-equal? (length parts) 4)
  (check-true (regexp-match? #px#"name=\"request\"" (cadr parts)))
  (check-true (regexp-match? #px#"application/json\r\n\r\n" (cadr parts)))
  (check-true (regexp-match? #px#"name=\"asset-0\"; filename=\"asset-0\"" (caddr parts)))
  (check-true (regexp-match? #px#"image/png" (caddr parts)))
  ;; the raw bytes are in there unharmed
  (check-true (regexp-match? (byte-regexp (tiny-png)) (caddr parts))))

;; ---- gzip --------------------------------------------------------------------------

(test-case "gzip-bytes round-trips through the standard inflater"
  (define data (make-bytes 4096 65))
  (define zipped (gzip-bytes data))
  (check-true (< (bytes-length zipped) (bytes-length data)))
  (define out (open-output-bytes))
  (gunzip-through-ports (open-input-bytes zipped) out)
  (check-equal? (get-output-bytes out) data))
