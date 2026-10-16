#lang racket/base

;; private/jsonw.rkt — the request writer: ordered objects, images, non-finite
;; numbers.
;;
;; Three things `jsexpr->string` cannot do, each needed by the current API:
;;
;;   • ORDER. A choice's options and the questions map are JSON objects whose
;;     key order the model sees — measured on the open-weight checkpoint, 1 of 6
;;     option orderings agreed with the written order and a probability moved by
;;     0.54 — and the vendor's own SDK preserves the author's insertion order.
;;     A Racket hash cannot carry order, so an ordered object is `(jobj pairs)`:
;;     a struct, not a bare list of pairs, because a pair whose cdr is a list IS
;;     a list and the two shapes would be told apart by luck.
;;   • IMAGES. An `image` value may sit anywhere in state, instructions or a
;;     criteria description. It serializes as its alias (when the caller named
;;     it in #:assets) or its reference, and the writer reports every image it
;;     met so the client knows what to upload.
;;   • NON-FINITE NUMBERS. JSON has no NaN/Infinity tokens; the API's SDKs send
;;     them as the strings "NaN"/"Infinity"/"-Infinity", so this writer does too
;;     rather than emitting an invalid token or refusing a value the API takes.
;;
;; Plain hashes still serialize (state is the caller's jsexpr and its key order
;; was never promised); use jobj where order is meaning.

(require racket/list json "asset.rkt")

(provide (struct-out jobj) content->json content-ok?)

;; pairs : (listof (cons string content)) — written in list order
(struct jobj (pairs) #:transparent)

;; Is v something this writer can send? The one answer question.rkt and
;; client.rkt both ask, so it lives beside the writer it describes.
(define (content-ok? v)
  (cond [(or (string? v) (boolean? v) (eq? v 'null) (image? v)) #t]
        [(and (real? v) (or (exact-integer? v) (inexact? v))) #t]
        [(jobj? v) (andmap (lambda (p) (and (pair? p) (string? (car p)) (content-ok? (cdr p))))
                           (jobj-pairs v))]
        [(list? v) (andmap content-ok? v)]
        [(hash? v) (for/and ([(k x) (in-hash v)])
                     (and (symbol? k) (content-ok? x)))]
        [else #f]))

;; content aliases found -> string
;;   aliases : hasheq image -> alias string (images the caller named in #:assets)
;;   found   : a box of (listof image), newest first — every image that
;;             serialized as its REFERENCE, in order of first appearance once
;;             the caller reverses it. Pass (box '()) and read it after.
(define (content->json v aliases found)
  (define out (open-output-string))
  (emit v out aliases found)
  (get-output-string out))

(define (emit v out aliases found)
  (cond
    [(image? v)
     (define alias (hash-ref aliases v #f))
     (cond [alias (write-json alias out)]
           [else
            (unless (memq v (unbox found))
              (set-box! found (cons v (unbox found))))
            (write-json (image-reference v) out)])]
    [(string? v) (write-json v out)]
    [(boolean? v) (write-json v out)]
    [(eq? v 'null) (write-string "null" out)]
    [(real? v)
     (cond [(exact-integer? v) (write-json v out)]
           [(eqv? v +inf.0) (write-json "Infinity" out)]
           [(eqv? v -inf.0) (write-json "-Infinity" out)]
           [(and (inexact? v) (not (= v v))) (write-json "NaN" out)]
           [(inexact? v) (write-json v out)]
           [else (error 'jev "cannot serialize the exact non-integer ~e; make it inexact" v)])]
    [(jobj? v)
     (write-string "{" out)
     (for ([p (in-list (jobj-pairs v))] [i (in-naturals)])
       (when (> i 0) (write-string "," out))
       (write-json (car p) out)
       (write-string ":" out)
       (emit (cdr p) out aliases found))
     (write-string "}" out)]
    [(list? v)
     (write-string "[" out)
     (for ([x (in-list v)] [i (in-naturals)])
       (when (> i 0) (write-string "," out))
       (emit x out aliases found))
     (write-string "]" out)]
    [(hash? v)
     ;; a plain hash never promised an order; what matters is that its VALUES
     ;; still go through this writer, so an image three levels into state works
     (write-string "{" out)
     (for ([p (in-list (hash-map v cons #t))] [i (in-naturals)])
       (when (> i 0) (write-string "," out))
       (write-json (symbol->string (car p)) out)
       (write-string ":" out)
       (emit (cdr p) out aliases found))
     (write-string "}" out)]
    [else (error 'jev "cannot serialize ~e in a request" v)]))
