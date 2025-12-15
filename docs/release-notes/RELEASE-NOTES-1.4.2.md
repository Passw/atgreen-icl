# ICL 1.4.2 Release Notes

## Bug Fixes

### configure-lisp Keyword Handling
- Fixed type error when registering new Lisp implementations with keyword names
- `(configure-lisp :chez :program "/usr/bin/chez")` now works correctly

### Portable Program Detection
- Replaced shell-based `which` command with portable PATH search
- Uses `uiop:getenv-absolute-directories` to search PATH directories
- On Windows, checks for `.exe`, `.cmd`, `.bat`, and `.com` extensions

## New Features

### *use-multiline-editor* Configuration
- Added `*use-multiline-editor*` variable (exported, default T)
- Set to NIL in `~/.iclrc` to force simple line-based input
- Useful for terminals where the multiline editor has issues

## Example Configuration

```lisp
;; ~/.iclrc

;; Disable multiline editor
(setf icl:*use-multiline-editor* nil)

;; Register a custom Lisp implementation
(icl:configure-lisp :chez
  :program "/opt/chez/bin/chez"
  :args '("--quiet"))
```

## Breaking Changes

None.
