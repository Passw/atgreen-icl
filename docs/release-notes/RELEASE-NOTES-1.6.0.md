# ICL 1.6.0 Release Notes

## New Features

### Interactive Object Inspector
- New TUI for exploring Lisp objects interactively
- Navigate with arrow keys or j/k (vim-style)
- Drill into nested objects with Enter, go back with b
- `,i` alone inspects the last result (`*`)
- `,inspect <expr>` or `,i <expr>` to inspect any expression
- `,inspect-static` available for non-interactive output

### Filesystem Path Completion
- Tab completion now works for filesystem paths inside strings
- Automatically detects pathname contexts:
  - `#p"..."` pathname literals
  - Known pathname functions (`load`, `open`, `with-open-file`, etc.)
  - Path-like strings starting with `/`, `~/`, or `./`
- Platform-aware: Windows patterns (`C:\`) only matched on Windows

## Documentation

- Updated README with interactive inspector documentation and keyboard shortcuts

## Breaking Changes

None.
