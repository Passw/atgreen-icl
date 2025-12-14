# ICL 1.1.0 Release Notes

## Summary

This release focuses on improved user experience with a cleaner interface, faster startup, and several bug fixes.

## New Features

- **Syntax highlighting** - Real-time colorization of Lisp code as you type, with support for strings, numbers, symbols, keywords, and comments
- **Paren matching** - Matching parentheses are highlighted as you type, with distinct colors for matches and mismatches
- **Startup spinner** - Animated spinner displayed while waiting for the Lisp backend to initialize
- **Dimmed prompts** - Package name and continuation prompts now use a subtle dark gray color for reduced visual noise
- **NO_COLOR support** - Respects the [NO_COLOR](https://no-color.org/) environment variable to disable colored output
- **Alt+Enter for newlines** - Press Alt+Enter to insert a literal newline in multi-line input (works in most terminals)
- **Shift+Enter for newlines** - Press Shift+Enter to insert a literal newline (requires kitty keyboard protocol support)

## Improvements

- **Faster startup** - Slynk now uses ASDF for loading with FASL caching, significantly reducing startup time on subsequent runs
- **Cleaner startup** - Removed verbose "Starting..." and "Connected..." messages
- **Simplified codebase** - Removed vestigial `*use-slynk*` variable; ICL now always uses the Slynk backend

## Bug Fixes

- **Fixed auto-indentation** - Corrected indentation calculation that was causing excessive indentation in multi-line input

## Keyboard Shortcuts (Updated)

| Key | Description |
|-----|-------------|
| `Alt+Enter` | Always insert newline (works in most terminals) |
| `Shift+Enter` | Always insert newline (requires kitty keyboard protocol) |
| `Enter` | Submit form if complete, otherwise insert newline |

## Environment Variables (New)

| Variable | Description |
|----------|-------------|
| `NO_COLOR` | When set to any non-empty value, disables colored output |

## Installation

### Fedora/RHEL
```bash
sudo dnf install ./icl-1.1.0-1.*.x86_64.rpm
```

### Debian/Ubuntu
```bash
sudo apt install ./icl_1.1.0-1_amd64.deb
```

## Upgrading from 1.0.0

No breaking changes. Simply install the new version over the previous one.
