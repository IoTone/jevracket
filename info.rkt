#lang info

;; jev — a Racket client for the TypeSafe "System One" API.
;;
;; Single-collection package: `(require jev)` after `raco pkg install`, or
;; `(require "<path>/jevracket/main.rkt")` straight out of a checkout. Both work;
;; nothing here depends on being installed.

(define collection "jev")
(define pkg-desc "Racket client for the TypeSafe System One (Jev) API: typed questions, calibrated answers")
(define version "0.1.0")
(define pkg-authors '("IoTone, Inc."))
(define license 'MIT)

;; Deliberately `base` only. Everything this needs — net/http-client, json,
;; racket/contract — ships with Racket. A client for one HTTP endpoint should not
;; drag a dependency tree behind it.
(define deps '("base"))
(define build-deps '("rackunit-lib"))

;; `raco test` walks the package; these two are not unit tests. mock-api is a
;; SERVER (it blocks), live-smoke spends real money against the real API.
(define test-omit-paths '("test/mock-api.rkt" "test/live-smoke.rkt"))
