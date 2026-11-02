# Examples

Small programs that run jev against a **local Laya server**. Laya is a
self-hosted System One engine. Its server speaks TypeSafe's `POST /v1/systemone`
wire, so nothing leaves your machine and nothing costs money.

| File | Shows |
|---|---|
| `laya.rkt` | The shared client: local address, compression off, dummy key |
| `triage.rkt` | A noul, a choice and a score about one message, in one request |
| `confidence-gate.rkt` | Act above a confidence threshold, route to a human below it |
| `review-batch.rkt` | Structured state (one JSON record per request) and timing |

## 1. Start a Laya server

On a Mac, use [laya-mlx](https://github.com/mizorewww/laya-mlx), which runs on
the GPU. From the repository root:

```sh
uv venv -p 3.12 .laya && VIRTUAL_ENV=$PWD/.laya uv pip install 'laya-mlx[serve]'
LAYA_HOST=127.0.0.1 LAYA_PORT=8765 LAYA_MODELS=english .laya/bin/laya-mlx-serve
```

The first start downloads the English checkpoint (about 420 MB). The server is
ready when it prints `Uvicorn running on http://127.0.0.1:8765`. Keep
`LAYA_HOST=127.0.0.1`: the default is `0.0.0.0`, and without `LAYA_API_KEY` the
server answers anyone on the network. Don't pass `--help`: `laya-mlx-serve` has
no flags, and any argument starts the server.

On Linux, the same steps work with `laya[serve]` and `laya-serve`.

The examples expect `http://127.0.0.1:8765`. Set `LAYA_BASE_URL` to use another
address.

## 2. Run from Emacs (racket-mode)

Start the server in Emacs with `M-x async-shell-command` or in a `vterm` buffer,
then open an example:

| Key | Does |
|---|---|
| `F5` (`fn-F5` on a Mac) | `racket-run-and-switch-to-repl`: runs the file plus its `test` and `main` submodules (the demo), then switches to the REPL |
| `C-c C-c` | `racket-run-module-at-point`: runs only the module around the cursor. Outside `(module+ main ...)` you get a REPL but no demo. Inside it, the demo runs |
| `C-x C-e` | Sends the expression before point to the REPL |
| `C-M-x` | Sends the top-level form at point |

After a run, the REPL is inside the module, so its definitions are yours to call:

```racket
> (show (triage "My card was charged twice for one order."))
> (route "Can I change my delivery address?")
> (ask/raw laya "Where is my order?" (list (cons "late" (noul "The order is late."))))
```

Edit the file and press `F5` again to reload. (These bindings are from
racket-mode 2026-09; older versions bound `C-c C-c` to `racket-run`, which also
ran `main`.)

**If `C-c C-c` says "No Geiser REPL for this buffer".** Geiser has claimed the
file. With `geiser-racket` installed, `.rkt` files open in `scheme-mode` with
Geiser, even when racket-mode is installed too. Run `M-x racket-mode` for the
current buffer, or add this to your init file to fix it permanently:

```elisp
;; .rkt files use racket-mode, not Geiser's scheme-mode. `push', not
;; `add-to-list': racket-mode's own entry is already in the list (behind
;; Geiser's), so add-to-list would leave it there and change nothing.
(push '("\\.rkt\\'" . racket-mode) auto-mode-alist)
;; GUI Emacs on macOS may not have /opt/homebrew/bin (or similar) on its path
(setq racket-program "/Applications/Racket v9.2/bin/racket")   ; adjust to your install
```

The mode line should then say `Racket` when you open an example.

**Environment variables.** The Racket back end inherits Emacs's environment from
when it started, and a GUI Emacs on macOS doesn't see your shell's variables.
To set `LAYA_BASE_URL`, run `M-x setenv`, then `M-x racket-stop-back-end`, then
run the file again. The `exec-path-from-shell` package fixes this permanently.

## 3. Or from a terminal

```sh
racket examples/triage.rkt
racket examples/confidence-gate.rkt
racket examples/review-batch.rkt
```

## What to expect from Laya

These are notes from running the examples against laya-mlx 0.6.0 on the
English checkpoint:

- **Speed:** about 15 ms per request on an Apple GPU.
- **Choice questions** are its strength. Intent and routing answers were sensible,
  and their confidence separated clear messages from vague ones.
- **Noul and score questions were weaker.** "Payouts have been failing for three
  days and I am losing sales" scored 0.14 on time pressure, and the anger score
  sat near 1.2 of 2 for every message, calm or not. The review sentiment and defect
  answers were good, so phrasing matters. Test a question on your own data
  before you trust it.
- **Two confidences.** `ask` returns Laya's own `confidence` (normalized entropy).
  The Jev-equivalent `x_jev_confidence` comes back only in `ask/raw`, which
  `confidence-gate.rkt` shows how to read.
- **The limits of Laya's API:** every question needs `instructions`, there are
  no images, and `list-models` returns 404.
