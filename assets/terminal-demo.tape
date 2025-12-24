# ICL Terminal Demo - Full Feature Showcase
# Run with: vhs terminal-demo.tape

Output assets/terminal-demo.gif

Set Shell "bash"
Set FontSize 14
Set Width 900
Set Height 600
Set Theme "Dracula"
Set TypingSpeed 75ms

# Skip terminal background query (VHS doesn't support it)
Env ICL_BACKGROUND "dark"

# ═══════════════════════════════════════════════════════════════
# SECTION 1: Basic Usage & Syntax Highlighting
# ═══════════════════════════════════════════════════════════════

Type "icl"
Enter
Sleep 3s

# Simple expression - watch the paren matching!
Type "("
Sleep 200ms
Type "+ 1 2 3"
Sleep 400ms
Type ")"
Sleep 600ms
Enter
Sleep 1s

# Define a function - show multi-line and paren matching
Type "(defun greet "
Sleep 300ms
Type "("
Sleep 200ms
Type "name"
Sleep 200ms
Type ")"
Sleep 500ms
Enter
Sleep 400ms
Type "  (format nil "
Sleep 300ms
Type '"Hello, ~A!" '
Sleep 300ms
Type "name"
Sleep 300ms
Type ")"
Sleep 500ms
Type ")"
Sleep 600ms
Enter
Sleep 1s

# Call our function
Type '(greet '
Sleep 200ms
Type '"World"'
Sleep 300ms
Type ")"
Sleep 500ms
Enter
Sleep 1.5s

# ═══════════════════════════════════════════════════════════════
# SECTION 2: Tab Completion
# ═══════════════════════════════════════════════════════════════

Type ";; Tab completion"
Enter
Sleep 800ms

Type "(string-"
Sleep 600ms
Tab
Sleep 1s
Down
Sleep 400ms
Down
Sleep 400ms
Down
Sleep 400ms
Tab
Sleep 600ms
Type ' "hello" '
Sleep 300ms
Type '"world"'
Sleep 300ms
Type ")"
Sleep 500ms
Enter
Sleep 1.5s

# Load alexandria for package completion demo
Type ",ql alexandria"
Enter
Sleep 3s

# Package-qualified completion
Type "(alexandria:"
Sleep 500ms
Type "last"
Sleep 400ms
Tab
Sleep 1s
Tab
Sleep 500ms
Type " '"
Sleep 200ms
Type "(1 2 3)"
Sleep 300ms
Type ")"
Sleep 500ms
Enter
Sleep 1.5s

# ═══════════════════════════════════════════════════════════════
# SECTION 3: History Search (Ctrl+R)
# ═══════════════════════════════════════════════════════════════

Type ";; History search with Ctrl+R"
Enter
Sleep 800ms

Ctrl+R
Sleep 600ms
Type "greet"
Sleep 1s
Enter
Sleep 600ms
Enter
Sleep 1.5s

# ═══════════════════════════════════════════════════════════════
# SECTION 4: Inspector
# ═══════════════════════════════════════════════════════════════

Type ";; Interactive inspector"
Enter
Sleep 800ms

Type "(defvar *data* '"
Sleep 300ms
Type "("
Sleep 200ms
Type ":name "
Sleep 200ms
Type '"Alice" '
Sleep 300ms
Type ":scores "
Sleep 200ms
Type "("
Sleep 200ms
Type "95 87 92"
Sleep 300ms
Type ")"
Sleep 400ms
Type ")"
Sleep 400ms
Type ")"
Sleep 500ms
Enter
Sleep 1s

Type ",inspect *data*"
Enter
Sleep 1.2s

# Navigate inspector slowly
Sleep 600ms
Down
Sleep 500ms
Down
Sleep 500ms
Enter
Sleep 1s

# Drill into nested structure
Down
Sleep 500ms
Enter
Sleep 1s

# Go back
Sleep 500ms
Type "b"
Sleep 700ms
Type "b"
Sleep 700ms

# Quit inspector
Type "q"
Sleep 1.2s

# ═══════════════════════════════════════════════════════════════
# SECTION 5: Error Handling & AI Explain
# ═══════════════════════════════════════════════════════════════

Type ";; Error handling"
Enter
Sleep 800ms

# Call greet with wrong number of arguments
Type "("
Sleep 200ms
Type "greet "
Sleep 300ms
Type '"a" '
Sleep 200ms
Type '"b" '
Sleep 200ms
Type '"c"'
Sleep 300ms
Type ")"
Sleep 500ms
Enter
Sleep 1.2s

# Ask AI to explain the error
Type ",explain"
Enter
Sleep 18s

# Exit
Type ",quit"
Enter
Sleep 1s
