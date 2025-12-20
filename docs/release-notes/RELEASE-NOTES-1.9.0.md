# ICL 1.9.0 Release Notes

## New Features

### Web-Based System Browser

A new VS Code-style system browser accessible via `icl --browser` or the `,browser` command. Features include:

- **Dockable Panel Layout**: Packages, Symbols, and Symbol Info panels arranged above a full xterm.js REPL terminal
- **Hover-to-Inspect**: Hover over symbols in the REPL to highlight them; click to navigate to their definition in the browser panels
- **Multi-Inspector Support**: Open multiple inspector tabs to compare objects side-by-side
- **Text Selection**: Click and drag to select text in the browser REPL for copy/paste
- **Auto-Refresh**: Package and symbol lists automatically refresh after REPL evaluation
- **Persistent History**: Browser REPL shares command history with terminal sessions

### Comprehensive Theming System

Full theming support for both terminal and browser interfaces:

- **12 Built-in Themes**: Dracula, Nord, One Dark, Gruvbox Dark, Tokyo Night, Catppuccin Mocha, Solarized Dark, Monokai, GitHub Light, Solarized Light, Gruvbox Light, and One Light
- **Dark Mode Detection**: Automatically selects appropriate theme based on system dark/light preference
- **Independent Theme Selection**: Terminal and browser can use different themes
- **Custom Theme Support**: Define your own themes in `~/.config/icl/config.lisp`
- **`,theme` Command**: List available themes and switch between them

### CLI Improvements

- **`-b/--browser` Flag**: Launch ICL with the browser interface directly from command line

## Improvements

- Symbol clicks in the browser now show all bindings (class, function, variable) consistently
- Clicking a symbol in the REPL clears filters and scrolls to the symbol in the browser panels
- Added architecture diagram to README showing browser interface components
- Added AGENTS.md for AI assistant context

## Bug Fixes

- Fixed browser output race conditions that could cause garbled display
- Fixed terminal rendering race condition at browser startup
- Fixed Symbol Info panel hanging on complex values (added print limits)
- Fixed color codes appearing in output ("244" prefix issue)
- Fixed filter values being reset when package/symbol lists refresh
- Limited browser to single connection to prevent conflicts

## Breaking Changes

None.
