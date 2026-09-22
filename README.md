# jev

A Racket client for the [TypeSafe](https://typesafe.ai) **System One** API.

Jev answers typed questions about a blob of state and returns calibrated
probabilities. You do not get prose back and you do not parse anything: a yes/no
question returns a number between 0 and 1, a multiple-choice question returns the
winner plus the whole distribution, and a rating question returns a
probability-weighted value plus a confidence.

```racket
(require jev)

(define c (make-client))                      ; reads TYPESAFE_API_KEY

(define r
  (ask c "Payouts have been failing for three days and I am losing sales."
       (hash "urgent" (noul "The message conveys time pressure.")
             "team"   (choice "Which team should handle this"
                              (hash "billing"   "Payments, invoicing, refunds"
                                    "technical" "Bugs, outages, integrations"))
             "anger"  (score "How angry the writer sounds"
                             '("Calm" "Annoyed" "Furious")))))

(noul-answer-value   (response-ref r "urgent"))   ; 0.99
(choice-answer-value (response-ref r "team"))     ; "technical"
(answer-confidence   (response-ref r "team"))     ; 0.9
(score-answer-value  (response-ref r "anger"))    ; 1.0
(response-model r)                                ; "jev-1.13.0"
```

All three questions above cost **one** request. Jev reads the state once and
evaluates every question against it, so batching is both cheaper and faster than
asking separately.

## Built from the spec, not ported

This is written against the published HTTP reference at
<https://docs.typesafe.ai/api>, not translated from the vendor's Python SDK.
The wire protocol is two endpoints and a handful of JSON shapes; the SDK's bulk
is ergonomics that do not survive a move to Racket anyway. Building from the spec
keeps the provenance clean and leaves us independent of an SDK that is still
pre-1.0.

The tests pin the documented request and response examples verbatim, so a change
on the vendor's side fails the suite rather than surfacing in production.

## Requirements

Racket 8.x or newer with working TLS; developed and tested on 9.3. **No
dependencies beyond `base`** — `net/http-client` and `json` ship with Racket. A
client for one HTTP endpoint should not drag a dependency tree behind it.

Install from the [official download](https://download.racket-lang.org/). It is the
newest build and needs no root:

```sh
curl -fLO https://download.racket-lang.org/installers/9.3/racket-9.3-x86_64-linux-cs.sh
sh racket-9.3-x86_64-linux-cs.sh --in-place --dest ~/.local/racket
ln -sfn ~/.local/racket/bin/racket ~/.local/racket/bin/raco ~/.local/bin/
```

The [Snap Store](https://snapcraft.io/racket) and Nix (`nix-shell -p racket`)
builds both work; the snap trails the release by a version or two.

**Do not install Racket from Homebrew or Linuxbrew.** The `minimal-racket` formula
resolves OpenSSL through the binary's `DT_RPATH`, and when that bottle's OpenSSL is
built against a newer glibc than the host provides, the library never loads. Racket
then reports TLS as simply unavailable and every request dies at connect time:

```
ssl-connect: requested protocol not supported;
 SSL not available; check `ssl-load-fail-reason'
```

`DT_RPATH` outranks `LD_LIBRARY_PATH`, so no environment variable will redirect it.
Check an install before blaming the transport:

```sh
racket -e '(require openssl) (printf "~s ~s\n" ssl-available? ssl-load-fail-reason)'
# want: #t #f
```

Minimality is not the problem — the official "Minimal Racket" download is fine,
since this library needs only `base`. It is Homebrew's packaging that breaks.

## Install

```sh
raco pkg install --link /path/to/jevracket   # then (require jev)
```

Or skip installing and require it by path, which is what a host application
embedding this will usually do:

```racket
(require "../jevracket/main.rkt")            ; relative path
(require (file "/opt/jevracket/main.rkt"))   ; absolute needs the (file ...) form
```

A bare absolute string is not a valid Racket module path, so an embedding host
with an absolute location must wrap it in `(file ...)`.

`choice` and `score` are common words. If they collide, prefix the module:

```racket
(require (prefix-in jev: jev))
(jev:ask c state (hash "team" (jev:choice ...)))
```

## The three primitives

| | Returns | Use it when |
|---|---|---|
| `(noul instructions #:yes #:no)` | probability of yes, 0 to 1 | the question has two answers |
| `(choice instructions criteria)` | winner, per-option probabilities, confidence | one of a closed set, up to 255 |
| `(score instructions criteria)` | weighted value, per-level probabilities, confidence | an ordered rubric, 2 to 10 levels |

`criteria` for a choice is a map of option to rubric, or `#f` where an option
needs no explanation. An association list works too and keeps your ordering:

```racket
(choice "Route this ticket"
        '(("billing"   . "Payments, invoicing, refunds")
          ("technical" . "Bugs, outages, integrations")
          ("other"     . #f)))
```

`criteria` for a score is an ordered list, lowest level first. The order **is**
the scale, which is the whole difference between a score and a choice.

`instructions` may also be a JSON object or array when the question needs to
carry reference data:

```racket
(noul (hasheq 'candidate (hasheq 'name "John Smith" 'employer "Google")
              'question "Is this resume the same person as `candidate`?"))
```

Every documented limit is enforced at construction, so a bad question raises in
Racket with a source location instead of costing a round trip and a 422.

## Reading answers

Use the typed accessors when you know the shape, or the three generic ones
(`answer-value`, `answer-confidence`, `answer-probabilities`) when you are
routing generically.

Two places this client deliberately improves on the wire:

- A score's `legend` and `probabilities` arrive keyed by **integer** level, not
  by the numeric strings JSON uses. They index a list you wrote, so you can do
  arithmetic on them.
- A choice's probability keys stay **strings**, because they are option names you
  chose. Handing back the symbols the JSON parser happened to produce would leak
  the parser into your contract.

`answer-confidence` is `#f` for a noul. That is the API's shape, not an omission:
a noul's probability already is its certainty. Treat `#f` as "not applicable",
never as "unconfident".

**Confidence is the second axis.** The answer tells you what; the confidence
tells you whether to act. The usual shape is to act above a threshold and route
to a human below it.

## Errors

Everything raises a subtype of `exn:fail:jev`, split by what you can do about it.

```racket
(with-handlers ([exn:fail:jev:rate-limit?  (lambda (e) (defer (exn:fail:jev:rate-limit-retry-after-ms e)))]
                [exn:fail:jev:auth?        (lambda (e) (alert-operator!))]
                [exn:fail:jev:response?    (lambda (e) (log-bad-field (exn:fail:jev:response-field-path e)))]
                [exn:fail:jev:connection?  (lambda (e) (fall-back))])
  (ask c state qs))
```

- `exn:fail:jev:api` — the server answered unhappily. Carries `status`, the
  parsed `body`, `headers`, and `endpoint`. `(api-error-request-id e)` pulls the
  `x-typesafe-request-id` worth quoting in a bug report. Subtypes:
  `bad-request` 400, `auth` 401, `permission` 403, `not-found` 404,
  `unprocessable` 422, `rate-limit` 429, `server` 5xx.
- `exn:fail:jev:connection` and `:timeout` — no HTTP response at all.
- `exn:fail:jev:response` — a 2xx whose body broke the contract. `field-path`
  says where, e.g. `answers.tone.confidence`.

That last one is the point of the library. A missing `confidence` is never
returned as `#f` for the caller to trip over later; a half-built answer is worse
than a raised one.

An API key never appears in an exception message.

## Retries and timeouts

Defaults match the vendor SDK, so behaviour does not depend on which client you
used: 2 retries after the first attempt, 0.5s doubling to 5s with 25% jitter,
retry on 408, 429 and every 5xx (which covers 529 Overloaded), honour
`Retry-After`, give up after 30 seconds overall, 10 seconds per attempt.

```racket
(make-client #:timeout 5.0
             #:retry (retry-policy 3 0.5 5.0 0.25 retryable-api-status? #t #t 20.0))
```

`Retry-After` wins over the computed backoff and is clamped to the remaining
budget. The deadline is checked *before* sleeping, so a client never naps five
seconds only to give up on waking.

Each attempt runs under its own custodian, so abandoning a stalled request also
closes its socket. Racket's `http-sendrecv` has no timeout of its own, and
without this a single stuck connection hangs the calling thread forever.

## Configuration

| Variable | Default | |
|---|---|---|
| `TYPESAFE_API_KEY` | none | required; `make-client` refuses without it |
| `TYPESAFE_BASE_URL` | `https://api.typesafe.ai` | |
| `TYPESAFE_DEFAULT_MODEL` | `jev-latest` | |

`jev-latest` is an alias and it moves when the vendor ships. The response reports
the versioned id that actually answered — log it. If you have tuned confidence
thresholds, pass a pinned id such as `"jev-1.13.0"` and move on your own
schedule.

## Tests

```sh
raco test test/*-tests.rkt        # 45 cases, no network
racket test/live-smoke.rkt        # one real request; needs a key
```

The unit suite runs the client against a real socket
(`test/mock-api.rkt`, a scriptable HTTP server) rather than a stubbed transport,
because the bugs worth catching live in the transport: a header the client
forgets, a `Content-Length` it never sets, a 401 retried three times before
failing.

`test/live-smoke.rkt` reads `TYPESAFE_API_KEY` or falls back to
`~/.typesafe-key`, exercises all three primitives in one request plus the models
endpoint and the 401 path, and exits non-zero on failure.

## Before you put this on a production path

Findings from evaluating the service, not from using this client:

- **It is hosted only.** There is no self-hosted Jev. Unlike an
  OpenAI-compatible model endpoint, which an operator can point at a local
  server, enabling this means state leaves the machine. Zero data retention is
  an enterprise-plan option; the default is not ZDR.
- **English is where it is strongest.** The vendor says other languages,
  including CJK, are handled but not equally well. In our own testing it still
  separated good from bad Japanese translations cleanly, but with a narrower
  margin than English work. Measure on your own content before you trust a
  threshold.
- **Question design is a real discipline.** A plausible question can confidently
  answer the wrong thing. Asking "estimate this company's revenue" about a
  submission that *claims* a revenue figure returns the claim, with high
  confidence. Say what you mean.
- **Cost is not the constraint.** Input runs $0.042 per million tokens and output
  is free, so a five-hundred-string quality pass costs about a cent. Latency
  measured 120 to 260ms per request.
- **State is capped** at 32k tokens for the state plus the longest question, and
  64k for everything together. Long documents need chunking.

## License

MIT. Copyright IoTone, Inc. See `LICENSE`.
