# ICL 1.8.2 Release Notes

## Enhancements

### Tab Completion
- **Package name completion**: Tab completion now includes package names with a `:` suffix, making it easier to type qualified symbols. For example, typing `oc<TAB>` will offer `ocicl-runtime:` as a completion option.

### Roswell Support
- **Roswell/Quicklisp installation**: ICL now works correctly when installed via Roswell without requiring ocicl. The setup process detects the installation method and configures paths appropriately.

## Bug Fixes

- **Path resolution**: Fixed `icl-dir` path resolution for Roswell installations
- **Output formatting**: Fixed blank line appearing between input and result

## Breaking Changes

None.
