#lang racket/base

;; private/asset.rkt — image assets and the multipart body that carries them.
;;
;; An image rides OUTSIDE the JSON: the request becomes multipart/form-data with
;; the JSON as a part named "request" and each image as its own raw-bytes part,
;; which the JSON's `assets` object names. Base64-in-JSON exists on the API but
;; triples the bytes for nothing; this client only speaks the multipart form.
;;
;; An `image` value is a promise of bytes, not bytes: a path is read ONCE, on
;; first use, and the same image value can ride any number of requests without
;; re-reading the file. The content type is sniffed from the file signature
;; (PNG, JPEG) purely to LABEL the upload part — the API detects formats itself,
;; so an unrecognized image still goes, labeled as generic binary data. That is
;; the vendor SDK's rule and it is the right one: refusing a format here would
;; mean maintaining a list the server does not consult.
;;
;; NOTE the API currently gates image requests behind an explicit opt-in header,
;; `TypeSafe-Danger-Zone-Images: true` — the feature is experimental and the
;; vendor makes you say so. This client does not add it for you (the SDK does
;; not either); pass it via make-client's #:headers. See the README.

(require racket/random file/gzip)

(provide image image? image-from-base64 image-reference
         image-resolve! REFERENCE-RX
         encode-multipart gzip-bytes MIN-COMPRESSED-BODY-LENGTH)

;; source   : bytes, or a path to read lazily
;; resolved : #f until first use, then (cons content-type bytes)
;; ref      : "<image:32hex>" — the unique string that stands for this image
;;            inside request text; same shape as the vendor SDK's, so a body is
;;            scannable for references with one pattern whoever composed it
(struct img (source [resolved #:mutable] ref)
  #:methods gen:custom-write
  [(define (write-proc i port mode) (fprintf port "#<image ~a>" (img-ref i)))])

(define (image? v) (img? v))
(define (image-reference i) (img-ref i))

(define REFERENCE-RX #px"<image:[0-9a-f]{32}>")

(define (fresh-reference)
  (format "<image:~a>" (apply string-append
                              (for/list ([b (in-bytes (crypto-random-bytes 16))])
                                (format "~a~a" (number->string (quotient b 16) 16)
                                        (number->string (remainder b 16) 16))))))

;; bytes, or a path to an image file. A string is a PATH here — image bytes are
;; binary and arrive as `bytes?`; base64 text goes through image-from-base64,
;; which says what it is.
(define (image data)
  (unless (or (bytes? data) (path-string? data))
    (error 'image "expected image file bytes or a path to one, got ~e" data))
  (img data #f (fresh-reference)))

(define (image-from-base64 text)
  (unless (string? text)
    (error 'image-from-base64 "expected base64 text, got ~e" text))
  ;; wrapped base64 carries line breaks; they mean nothing
  (define squeezed (regexp-replace* #px"\\s+" text ""))
  (when (string=? squeezed "")
    (error 'image-from-base64 "image data must contain non-empty base64 text"))
  (unless (regexp-match? #px"^[A-Za-z0-9+/]*={0,2}$" squeezed)
    (error 'image-from-base64 "image data must contain valid base64 text"))
  (define decoded (base64-decode squeezed))
  (unless decoded
    (error 'image-from-base64 "image data must contain valid base64 text"))
  (image decoded))

;; net/base64's decoder ignores garbage rather than refusing it, so a strict
;; decode is spelled here: the regexp above has vetted the alphabet, this
;; handles the arithmetic.
(define (base64-decode s)
  (define (val c)
    (cond [(and (char>=? c #\A) (char<=? c #\Z)) (- (char->integer c) 65)]
          [(and (char>=? c #\a) (char<=? c #\z)) (+ 26 (- (char->integer c) 97))]
          [(and (char>=? c #\0) (char<=? c #\9)) (+ 52 (- (char->integer c) 48))]
          [(char=? c #\+) 62] [(char=? c #\/) 63]
          [else #f]))
  (define body (regexp-replace* #px"=+$" s ""))
  (define n (string-length body))
  (when (= 1 (remainder n 4)) (error 'image-from-base64 "image data must contain valid base64 text"))
  (define out (open-output-bytes))
  (let loop ([i 0] [acc 0] [bits 0])
    (cond [(= i n)
           (get-output-bytes out)]
          [else
           (define v (val (string-ref body i)))
           (define acc2 (+ (* acc 64) v))
           (define bits2 (+ bits 6))
           (cond [(>= bits2 8)
                  (write-byte (arithmetic-shift acc2 (- (- bits2 8))) out)
                  (loop (add1 i) (bitwise-and acc2 (sub1 (arithmetic-shift 1 (- bits2 8)))) (- bits2 8))]
                 [else (loop (add1 i) acc2 bits2)])])))

;; Content types recognized by file signature, to label the upload part.
(define SIGNATURES
  (list (cons #"\211PNG\r\n\32\n" "image/png")
        (cons #"\377\330\377" "image/jpeg")))
(define UNKNOWN-CONTENT-TYPE "application/octet-stream")

(define (sniff bs)
  (or (for/or ([sig (in-list SIGNATURES)])
        (and (>= (bytes-length bs) (bytes-length (car sig)))
             (bytes=? (subbytes bs 0 (bytes-length (car sig))) (car sig))
             (cdr sig)))
      UNKNOWN-CONTENT-TYPE))

;; -> (values content-type bytes). Reads a path once; raises on an unreadable
;; path or empty data, naming the image so a batch failure says which one.
(define (image-resolve! i)
  (unless (img-resolved i)
    (define data
      (let ([src (img-source i)])
        (if (bytes? src)
            src
            (with-handlers ([exn:fail:filesystem?
                             (lambda (e) (error 'image "image file ~a could not be read: ~a"
                                                src (exn-message e)))])
              (file->bytes* src)))))
    (when (zero? (bytes-length data))
      (error 'image "image data is empty (~a)" (img-ref i)))
    (set-img-resolved! i (cons (sniff data) data)))
  (values (car (img-resolved i)) (cdr (img-resolved i))))

(define (file->bytes* p)
  (call-with-input-file p (lambda (in) (port->bytes* in))))
(define (port->bytes* in)
  (let loop ([acc '()])
    (define b (read-bytes 65536 in))
    (if (eof-object? b)
        (apply bytes-append (reverse acc))
        (loop (cons b acc)))))

;; ---- multipart/form-data (RFC 7578) ---------------------------------------------

;; parts : (listof (list name content-type bytes filename-or-#f)); names and
;; filenames must be ASCII without quotes — they are written unescaped, which is
;; fine because every name this client writes is "request" or "asset-N".
;; -> (values content-type-string body-bytes)
(define (encode-multipart parts)
  (define boundary
    (apply string-append (for/list ([b (in-bytes (crypto-random-bytes 16))])
                           (format "~x~x" (quotient b 16) (remainder b 16)))))
  (define out (open-output-bytes))
  (for ([p (in-list parts)])
    (define name (car p)) (define ctype (cadr p))
    (define content (caddr p)) (define filename (cadddr p))
    (fprintf out "--~a\r\nContent-Disposition: form-data; name=\"~a\"~a\r\nContent-Type: ~a\r\n\r\n"
             boundary name
             (if filename (format "; filename=\"~a\"" filename) "")
             ctype)
    (write-bytes content out)
    (write-bytes #"\r\n" out))
  (fprintf out "--~a--\r\n" boundary)
  (values (format "multipart/form-data; boundary=~a" boundary)
          (get-output-bytes out)))

;; ---- request compression ---------------------------------------------------------

;; Bodies below this fit in a single TCP segment, so compressing them saves no
;; round trip and only costs CPU — the vendor SDK's threshold, kept.
(define MIN-COMPRESSED-BODY-LENGTH 1024)

;; The API accepts gzip or zstd on request bodies; Racket ships gzip, so gzip it
;; is. (The SDK sends zstd — either works, the server answers the same.)
(define (gzip-bytes bs)
  (define in (open-input-bytes bs))
  (define out (open-output-bytes))
  (gzip-through-ports in out #f (current-seconds))
  (get-output-bytes out))
