# ICL 1.11.0 Release Notes

## Highlights

**Interactive flame graph profiling.** Profile any Lisp form and visualize the results with Speedscope directly in the browser interface.

## New Features

### Flame Graph Profiling

Profile Lisp code and view interactive flame graphs:

- **`,flame (form)`** - Profile execution and open Speedscope visualization
- **Aliases**: `,flamegraph`, `,fg`
- **Interactive Visualization**: Zoom, pan, search, and explore profiles
- **Multiple Views**: Timeline, left-heavy, and sandwich views in Speedscope
- **Smart Filtering**: Automatically removes Slynk/RPC overhead frames

Example:
```lisp
ICL> ,flame (asdf:load-system :my-project)
```

### Speedscope Integration

Embedded Speedscope profiler viewer:

- **No External Dependencies**: Speedscope is embedded in the ICL binary
- **Browser Panel**: Opens in a Dockview panel alongside the terminal
- **Speedscope Format**: Profiles are converted to standard Speedscope JSON

## Technical Details

- Uses SBCL's `sb-sprof` statistical profiler
- Folded stack format converted to Speedscope JSON
- Profile data stored in memory (cleared on restart)
- Overhead frame filtering removes infrastructure noise

## Requirements

- SBCL (uses sb-sprof for profiling)
- Browser interface for visualization

## Breaking Changes

None. The binary is fully backwards compatible.
