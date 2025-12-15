;;; eval.lisp --- Evaluation for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defun update-history (form values)
  "Update history variables with new form and values."
  ;; Rotate result history (both icl-* and IRB-style _)
  (setf icl-*** icl-**
        icl-** icl-*
        icl-* (first values)
        ___ __
        __ _
        _ (first values))
  ;; Rotate input history
  (setf icl-+++ icl-++
        icl-++ icl-+
        icl-+ form)
  ;; Rotate values history
  (setf icl-/// icl-//
        icl-// icl-/
        icl-/ values)
  ;; Increment input count
  (incf *input-count*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Hooks
;;; ─────────────────────────────────────────────────────────────────────────────

(defun run-before-eval-hooks (form)
  "Run all before-eval hooks with FORM."
  (dolist (hook *before-eval-hook*)
    (handler-case
        (funcall hook form)
      (error (e)
        (format *error-output* "~&;; Warning: before-eval hook error: ~A~%" e)))))

(defun run-after-eval-hooks (form values)
  "Run all after-eval hooks with FORM and VALUES."
  (dolist (hook *after-eval-hook*)
    (handler-case
        (funcall hook form values)
      (error (e)
        (format *error-output* "~&;; Warning: after-eval hook error: ~A~%" e)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Safe Evaluation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun read-form (input)
  "Read a Lisp form from INPUT string.
   Returns the form, or signals a condition on error."
  (let ((*package* *icl-package*))
    (read-from-string input)))

(defun eval-and-print (input)
  "Parse, evaluate, and print results from INPUT string.
   Handles all errors gracefully. Uses Slynk backend for evaluation."
  ;; Try to read form locally for hooks/history, but don't fail if it doesn't work
  ;; (e.g., if the form uses packages only defined in the backend)
  (let ((form (ignore-errors (read-form input))))
    (when form
      (run-before-eval-hooks form))
    (handler-case
        (let ((result (backend-eval input)))
          ;; Update history/hooks with best-effort values
          (let ((values (cond
                          ((listp result) result)
                          ((null result) nil)
                          (t (list result)))))
            (when (and form values)
              (update-history form values))
            (when form
              (run-after-eval-hooks form values)))
          ;; Handle the result for display
          (cond
            ((null result)
             ;; No result - might be output already printed
             nil)
            ((stringp result)
             ;; listener-eval returns string representation
             (unless (string= result "")
               (format t "~&~A~A~%"
                       (colorize *result-prefix* *color-prefix*)
                       result)))
            ((listp result)
             ;; Structured result
             (print-values result))
            (t
             ;; Unexpected result type - print as-is
             (format t "~&~A~S~%" (colorize *result-prefix* *color-prefix*) result))))
      (undefined-function (e)
        (format *error-output* "~&Undefined function: ~A~%"
                (cell-error-name e)))
      (unbound-variable (e)
        (format *error-output* "~&Unbound variable: ~A~%"
                (cell-error-name e)))
      (error (e)
        ;; Give the process a moment to die (quit may be async)
        (sleep 0.1)
        ;; If backend connection lost or process died, exit the REPL quietly
        (unless (and *slynk-connected-p* (inferior-lisp-alive-p))
          (setf *slynk-connected-p* nil)
          (invoke-restart 'quit))
        ;; Print the error for other cases
        (format *error-output* "~&Error: ~A~%" e)
        ;; Hint about backtrace if available
        (when *last-error-backtrace*
          (format *error-output* "~&~A~%" (colorize "  Use ,bt for backtrace" *color-dim*)))
        ;; Optionally invoke error hook
        (when *error-hook*
          (funcall *error-hook* e))))))
