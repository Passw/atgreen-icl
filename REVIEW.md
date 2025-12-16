# ICL Code Review (2025-12-15)

## Quick Take
- Feature set is ambitious, but several platform/implementation claims in `README.md` are not actually supported by the current code.
- API surface exported from `icl` is slightly out of sync with the implementation.
- Startup path is SBCL-centric and uses SBCL/private Slynk internals, which harms portability and robustness.
- No automated tests or CI are present; regressions are likely as features grow.

## Findings
- **Exported but undefined API** – `*use-multiline-editor*` is exported from `src/package.lisp:24` but never defined anywhere else. This produces undefined symbol warnings for clients and leaves users without a documented way to force the simple input backend. *Fix:* add a `defvar` (with config plumbing) or drop the export to keep the public API accurate.
- **Type error when adding new Lisp implementations** – `configure-lisp` uses `(string-downcase impl)` when creating a new entry (`src/backend.lisp:39-63`). When `impl` is the documented keyword (e.g., `:chez`), this call signals a type error, so user-supplied implementations cannot be registered. *Fix:* use `(string-downcase (symbol-name impl))` (and consider normalizing to pathname designators).
- **Multi-implementation support is SBCL-only at runtime** – Despite advertising SBCL/CCL/ECL/CLISP/ABCL/Clasp, `start-inferior-lisp` falls into a hard error for any non-SBCL build because process spawning is wrapped in `#+sbcl` and the `#-sbcl` branch is `(error "Process spawning not implemented…")` (`src/backend.lisp:252-301`). Similarly, socket port probing (`port-in-use-p`) is SBCL-only. *Fix:* switch to `uiop:run-program`/`uiop:launch-program` and `usocket`/`uiop:tcp-connect` for portable spawning and port checks, or narrow the README claim.
- **Windows/portability regressions** – `program-exists-p` shells out to `which` (`src/backend.lisp:206-213`), which fails on Windows (where `where` is expected) and ignores `%PATH%` semantics. `port-in-use-p` is stubbed to `nil` on non-SBCL, so `find-free-port` can return an in-use port and immediately fail to connect. *Fix:* use `uiop:which-program` and a portable port probe (e.g., `usocket:socket-listen` with `:reuse-address t`).
- **Reliance on Slynk private interfaces** – The evaluation wrapper streams output via `slynk::send-to-emacs` and `slynk-backend:make-output-stream` (`src/slynk-client.lisp:71-113`). Those are internal symbols and can change across SLY releases, risking broken output streaming/backtrace capture. *Fix:* prefer the public RPCs (`slynk:eval-for-emacs`, `slynk:with-output-to-target`) or vendor the required helper into the injected init code to decouple from Slynk internals.
- **Build/test gaps** – Makefile hard-codes SBCL (`Makefile`) and assumes `ocicl` is installed, with no CI or automated tests. Given the amount of interactive parsing (history, completion, paredit, command dispatch), the absence of even lightweight unit tests (FiveAM/Rove) or a smoke test that spawns SBCL + Slynk makes regressions likely. *Fix:* add an ASDF test system, run it in CI (GitHub Actions), and include a minimal spawn/connect/eval round-trip.

## Recommended Next Steps
- Define or remove `*use-multiline-editor*`; wire it through `select-input-backend` so users can force the simple reader.
- Replace SBCL-specific process/port code with `uiop`/`usocket` (or document SBCL-only support and simplify the README).
- Fix `configure-lisp` keyword handling and `program-exists-p` portability.
- Refactor Slynk streaming to avoid private symbols; keep injected code version-pinned or vendored.
- Add a small automated test suite (command parsing, completion prefix classification, history encode/decode, backend round-trip) and run it in CI.
