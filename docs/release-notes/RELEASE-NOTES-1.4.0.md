# ICL 1.4.0 Release Notes

## New Features

### Paredit Mode
- Structural editing with balanced delimiters (auto-close parens, brackets, braces, quotes)
- Safe deletion that prevents unbalancing expressions
- Sexp navigation with `Alt+F` (forward) and `Alt+B` (backward)
- `,paredit [on/off]` command to toggle
- In paredit mode, Enter only submits when cursor is at end of buffer
- Banner shows `[paredit]` indicator when enabled
- Disabled by default; enable with `(setf icl:*paredit-mode* t)` in `~/.iclrc`

### Configuration Enhancements
- `configure-lisp` function to customize Lisp invocation (path, arguments)
- Config file now loads before backend starts, so `*default-lisp*` takes effect
- `,show-config` now displays `*default-lisp*` and `*paredit-mode*` options

### Bug Fixes
- Fixed output capture from inferior Lisp (`(format t "Hello")` now displays "Hello")
- Fixed `,apropos` to use standard CL functions instead of missing Slynk symbols

## Example Configuration

```lisp
;; ~/.iclrc

;; Use CCL as default
(setf icl:*default-lisp* :ccl)

;; Custom SBCL with more memory
(icl:configure-lisp :sbcl
  :program "/opt/sbcl/bin/sbcl"
  :args '("--dynamic-space-size" "8192"))

;; Enable paredit
(setf icl:*paredit-mode* t)
```

## Breaking Changes

None.
