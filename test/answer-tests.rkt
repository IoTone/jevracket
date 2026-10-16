#lang racket/base

;; test/answer-tests.rkt — parsing typed answers, and refusing untyped ones.
;;
;; Every happy-path case below is the literal JSON from the published API
;; reference, parsed with string->jsexpr so the test reads the vendor's bytes and
;; not our idea of them. The refusal cases are the contract this client exists to
;; keep: a 2xx that is missing a field is an error with a path, never a #f that
;; the caller discovers three functions later.
;;   raco test test/answer-tests.rkt

(require rackunit racket/string json
         "../private/answer.rkt" "../private/error.rkt")

(define (parse str) (parse-response (string->jsexpr str)))

;; ---- the reference responses --------------------------------------------------

(define NOUL-RESPONSE #<<JSON
{
  "model": "jev-1.13.0",
  "answers": {
    "is_urgent": {
      "type": "noul",
      "noul": 0.95
    }
  },
  "usage": { "input_tokens": 307, "output_tokens": 20 }
}
JSON
  )

(test-case "noul: the answer is a probability, and stays one"
  (define r (parse NOUL-RESPONSE))
  (check-equal? (response-model r) "jev-1.13.0")
  (define a (response-ref r "is_urgent"))
  (check-true (noul-answer? a))
  (check-equal? (answer-type a) 'noul)
  (check-equal? (noul-answer-value a) 0.95)
  (check-equal? (answer-value a) 0.95)
  ;; a noul carries no confidence — its probability already is its certainty
  (check-false (answer-confidence a))
  (check-false (answer-probabilities a))
  (check-equal? (usage-input-tokens (response-usage r)) 307)
  (check-equal? (usage-output-tokens (response-usage r)) 20))

(define CHOICE-RESPONSE #<<JSON
{
  "model": "jev-1.13.0",
  "answers": {
    "department": {
      "type": "choice",
      "choice": "billing",
      "probabilities": { "billing": 0.88, "technical": 0.12, "sales": 0.0 },
      "confidence": 0.81
    }
  },
  "usage": { "input_tokens": 318, "output_tokens": 34 }
}
JSON
  )

(test-case "choice: the winner plus the whole distribution, keyed by the caller's option names"
  (define a (response-ref (parse CHOICE-RESPONSE) "department"))
  (check-true (choice-answer? a))
  (check-equal? (choice-answer-value a) "billing")
  (check-equal? (answer-confidence a) 0.81)
  ;; STRINGS, not the symbols read-json happened to produce: these are names the
  ;; caller chose and handed us, and they should come back the way they went in
  (check-equal? (choice-answer-probabilities a)
                (hash "billing" 0.88 "technical" 0.12 "sales" 0.0))
  (check-equal? (hash-ref (answer-probabilities a) "sales") 0.0
                "a zero-probability option is still reported"))

(define SCORE-RESPONSE #<<JSON
{
  "model": "jev-1.13.0",
  "answers": {
    "frustration": {
      "type": "score",
      "score": 1.05,
      "legend": { "0": "Calm", "1": "Frustrated", "2": "Very angry" },
      "probabilities": { "0": 0.0, "1": 0.95, "2": 0.05 },
      "confidence": 0.92
    }
  },
  "usage": { "input_tokens": 304, "output_tokens": 18 }
}
JSON
  )

(test-case "score: levels arrive as integers, and a value may land between them"
  (define a (response-ref (parse SCORE-RESPONSE) "frustration"))
  (check-true (score-answer? a))
  (check-equal? (score-answer-value a) 1.05
                "1.05 is a real answer: mostly level 1, leaning 2")
  (check-equal? (answer-confidence a) 0.92)
  ;; the wire keys these by numeric STRING; they are level indices, so they are
  ;; integers here and a caller can do arithmetic on them
  (check-equal? (score-answer-legend a) (hash 0 "Calm" 1 "Frustrated" 2 "Very angry"))
  (check-equal? (score-answer-probabilities a) (hash 0 0.0 1 0.95 2 0.05))
  (check-equal? (hash-ref (score-answer-legend a) (inexact->exact (round (score-answer-value a))))
                "Frustrated"
                "the nearest level is reachable without string surgery"))

(test-case "several questions come back under the ids that were sent"
  (define r (parse #<<JSON
{"model":"jev-1.13.0",
 "answers":{"a":{"type":"noul","noul":0.1},
            "b":{"type":"noul","noul":0.9}},
 "usage":{"input_tokens":1,"output_tokens":2}}
JSON
                   ))
  (check-equal? (response-ids r) '("a" "b"))
  (check-equal? (answer-value (response-ref r "b")) 0.9)
  (check-exn #px"no answer named" (lambda () (response-ref r "missing"))))

;; ---- strictness ---------------------------------------------------------------

(define (field-path-of thunk)
  (with-handlers ([exn:fail:jev:response? exn:fail:jev:response-field-path]) (thunk) #f))

(test-case "a missing field raises, and names itself"
  (check-equal? (field-path-of
                 (lambda () (parse "{\"model\":\"m\",\"answers\":{\"t\":{\"type\":\"choice\",\"choice\":\"a\",\"probabilities\":{\"a\":1.0}}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}")))
                "answers.t.confidence")
  (check-equal? (field-path-of
                 (lambda () (parse "{\"model\":\"m\",\"answers\":{\"t\":{\"type\":\"noul\"}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}")))
                "answers.t.noul")
  (check-equal? (field-path-of (lambda () (parse "{\"answers\":{},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}")))
                "model")
  (check-equal? (field-path-of (lambda () (parse "[]"))) "(body)"))

(test-case "a choice that is not in its own distribution is refused"
  ;; one of the two fields is wrong and nothing here can tell which, so neither
  ;; can be trusted — exactly the case a hand-rolled parser would wave through
  (check-equal? (field-path-of
                 (lambda () (parse "{\"model\":\"m\",\"answers\":{\"t\":{\"type\":\"choice\",\"choice\":\"ghost\",\"probabilities\":{\"a\":1.0},\"confidence\":0.9}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}")))
                "answers.t.choice"))

(test-case "a score level with a probability but no description is refused"
  (check-equal? (field-path-of
                 (lambda () (parse "{\"model\":\"m\",\"answers\":{\"t\":{\"type\":\"score\",\"score\":1.0,\"legend\":{\"0\":\"low\"},\"probabilities\":{\"0\":0.5,\"1\":0.5},\"confidence\":0.9}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}")))
                "answers.t.legend.1"))

(test-case "an unknown answer type is skipped, and the rest of the response survives"
  ;; the vendor SDK's forward-compatibility rule: a new primitive arrives beside
  ;; answers you asked for by name, and failing the whole response over the one
  ;; you could not have constructed would take the readable ones down with it.
  ;; The raw body is still there via ask/raw. A malformed KNOWN type still
  ;; raises — that is a broken contract, not a new feature.
  (define r (parse "{\"model\":\"m\",\"answers\":{\"t\":{\"type\":\"vibes\",\"vibes\":1},\"ok\":{\"type\":\"noul\",\"noul\":0.5}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}"))
  (check-equal? (response-ids r) '("ok") "the unknown answer is not listed")
  (check-equal? (answer-value (response-ref r "ok")) 0.5)
  (check-exn #px"no answer named" (lambda () (response-ref r "t"))))

(test-case "a wrongly-typed field is as bad as a missing one"
  (check-equal? (field-path-of
                 (lambda () (parse "{\"model\":\"m\",\"answers\":{\"t\":{\"type\":\"noul\",\"noul\":\"very\"}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}")))
                "answers.t.noul")
  (check-equal? (field-path-of
                 (lambda () (parse "{\"model\":\"m\",\"answers\":{\"t\":{\"type\":\"score\",\"score\":1.0,\"legend\":{\"x\":\"low\"},\"probabilities\":{\"0\":1.0},\"confidence\":0.9}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}")))
                "answers.t.legend.x"
                "a level key that is not an index is malformed"))

;; ---- box (SDK 0.7.5) ------------------------------------------------------------

(define BOX-RESPONSE #<<JSON
{
  "model": "jev-1.14.0",
  "answers": {
    "mug": {
      "type": "box",
      "box": { "top": 0.21, "bottom": 0.56, "left": 0.4, "right": 0.62 },
      "confidence": 0.93
    }
  },
  "usage": { "input_tokens": 412, "output_tokens": 0 }
}
JSON
  )

(test-case "box: the located region arrives as fractions of the image"
  (define a (response-ref (parse BOX-RESPONSE) "mug"))
  (check-true (box-answer? a))
  (check-equal? (answer-type a) 'box)
  (define r (box-answer-value a))
  (check-equal? (box-region-top r) 0.21)
  (check-equal? (box-region-bottom r) 0.56)
  (check-equal? (box-region-left r) 0.4)
  (check-equal? (box-region-right r) 0.62)
  (check-equal? (answer-value a) r "the generic accessor answers the region")
  (check-equal? (answer-confidence a) 0.93)
  (check-false (answer-probabilities a)))

(test-case "box: a null confidence is #f — the one answer type where the wire allows it"
  (define a (response-ref (parse "{\"model\":\"m\",\"answers\":{\"mug\":{\"type\":\"box\",\"box\":{\"top\":0.1,\"bottom\":0.2,\"left\":0.3,\"right\":0.4},\"confidence\":null}},\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}") "mug"))
  (check-false (box-answer-confidence a))
  (check-false (answer-confidence a)))

(test-case "box: a missing edge raises with the path, like every known type"
  (check-equal? (field-path-of
                 (lambda () (parse "{\"model\":\"m\",\"answers\":{\"mug\":{\"type\":\"box\",\"box\":{\"top\":0.1,\"bottom\":0.2,\"left\":0.3}}},\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}")))
                "answers.mug.box.right"))

;; ---- usage may be partial (SDK 0.7.x) ---------------------------------------------

(test-case "usage counts the API did not report are #f, not an error"
  (define r (parse "{\"model\":\"m\",\"answers\":{\"a\":{\"type\":\"noul\",\"noul\":0.5}},\"usage\":{}}"))
  (check-false (usage-input-tokens (response-usage r)))
  (check-false (usage-output-tokens (response-usage r)))
  (define r2 (parse "{\"model\":\"m\",\"answers\":{\"a\":{\"type\":\"noul\",\"noul\":0.5}},\"usage\":{\"input_tokens\":null,\"output_tokens\":7}}"))
  (check-false (usage-input-tokens (response-usage r2)))
  (check-equal? (usage-output-tokens (response-usage r2)) 7))

;; ---- a structured legend echoes structured criteria (API 0.6+) --------------------

(test-case "a legend value may be the structured level the caller sent"
  (define a (response-ref (parse "{\"model\":\"m\",\"answers\":{\"s\":{\"type\":\"score\",\"score\":0.5,\"legend\":{\"0\":\"calm\",\"1\":{\"tone\":\"hostile\"}},\"probabilities\":{\"0\":0.5,\"1\":0.5},\"confidence\":0.6}},\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}") "s"))
  (check-equal? (hash-ref (score-answer-legend a) 1) (hasheq 'tone "hostile")))
