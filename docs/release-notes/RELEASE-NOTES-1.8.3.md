# ICL 1.8.3 Release Notes

## Bug Fixes

### Roswell Support
- **Roswell as a Lisp implementation**: Added `:roswell` as a proper Lisp implementation option. Roswell users experiencing SBCL core image mismatches can now use `icl --lisp roswell` or set `(setf icl:*default-lisp* :roswell)` in their config file. This launches the inferior Lisp via `ros run --` which uses Roswell's properly configured environment.

- **Auto-detection order**: Roswell is now checked first in the auto-detection order, so it will be preferred over system Lisps when available and no default is configured.

Fixes #7.

## Breaking Changes

None.
