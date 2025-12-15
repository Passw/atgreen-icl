# ICL 1.4.1 Release Notes

## Improvements

### Streaming Output
- Output from the inferior Lisp process now streams incrementally to the terminal
- Long-running operations like `(asdf:load-system ...)` display progress in real-time
- Uses Slynk's `:write-string` events for true streaming

### Backend Process Handling
- Clean exit when calling `(quit)` or `(sb-ext:exit)` in the backend
- ICL detects backend termination and exits gracefully without error messages
- Added timeout-based connection death detection to prevent hanging

### Backend-Only Packages
- Forms referencing packages only defined in the backend now work correctly
- Local read failures for hooks/history gracefully fall back to backend-only evaluation

## Bug Fixes

- Fixed buffered output that caused delays displaying inferior Lisp output
- Fixed REPL hanging when backend process terminates
- Fixed spurious error messages when user intentionally quits backend

## Breaking Changes

None.
