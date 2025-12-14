# ICL 1.1.2 Release Notes

## New Features

### Debugger Integration
- **Error backtraces**: When evaluation errors occur, ICL now captures the full SBCL backtrace
- **`,bt` command**: View the backtrace from the last error with `Use ,bt for backtrace` hint shown after errors
- **`,threads` command**: List all threads in the inferior Lisp with ID, name, and status

## Improvements

### Documentation
- Updated README with new features: syntax highlighting, parenthesis matching, debugger commands
- Added `ICL_BACKGROUND` environment variable documentation

### Build System
- Source packages (SRPM, Debian source) now include vendored ocicl libraries for offline builds

### Bug Fixes
- Fixed `,ls :all` filter to correctly show all symbols in a package
- Refactored eval to properly run before/after hooks with backend evaluation

## Commands Added

| Command | Description |
|---------|-------------|
| `,bt` | Show backtrace from last error |
| `,threads` | List all threads in inferior Lisp |
