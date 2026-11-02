#lang racket/base

;; examples/laya.rkt — the one client every example shares.
;;
;; A local Laya server (laya-serve, or laya-mlx-serve on a Mac) speaks the same
;; POST /v1/systemone wire as TypeSafe, with two settings that differ:
;;
;;   • it cannot read a gzipped body, so compression is off
;;   • it ignores the API key unless the server sets LAYA_API_KEY
;;
;; LAYA_BASE_URL overrides the address. In a GUI Emacs on macOS the Racket back
;; end may not see your shell's variables, which is why there is a default.

(require "../main.rkt")

(provide laya laya-base-url)

(define laya-base-url (or (getenv "LAYA_BASE_URL") "http://127.0.0.1:8765"))

(define laya
  (make-client #:base-url laya-base-url
               #:api-key (or (getenv "LAYA_API_KEY") "unused")
               #:compress? #f
               ;; a local model has no network to wait on; fail fast if it is down
               #:timeout 20.0))
