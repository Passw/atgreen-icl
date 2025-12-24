# ICL 1.14.0 Release Notes

## New Features

### Vega-Lite Visualization Support
- Added Vega-Lite chart rendering in the browser interface
- Automatic detection of Vega-Lite specs (by `$schema` or `mark`/`encoding` keys)
- Charts are responsive to panel resizing
- Theme-aware rendering (dark/light mode support)
- Use `,viz` command with Vega-Lite JSON strings

### Custom Visualizations
- New `icl-runtime:visualize` generic function for user-defined classes
- Define methods to return visualization specs for your own objects
- Supported types: `:html`, `:svg`, `:json`, `:vega-lite`, `:image-base64`
- See `examples/vega.lisp` for a complete example

### Ctrl-Z Suspend Support
- ICL now properly handles Ctrl-Z to suspend the process
- Terminal is restored before suspending
- Raw mode is re-entered and screen is redrawn on resume
- Fixes GitHub issue #11

## Documentation
- Added examples directory with `vega.lisp` demonstration
- Updated README with Data visualization and Custom visualizations features
- Added link to examples in Custom Visualizations documentation section
