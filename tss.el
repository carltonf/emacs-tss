;;; Main file of "tss" library
;;;
;;; TSS is TypeScript Service for Emacs.
;;; 
;;; It strives to serve as a middle layer between various typescript projects
;;; and typescript language service. TSS defines a common set of API that make
;;; 3rd party ELisp packages easily understand ts projects and source code.
;;;
;;; TSS-IDE is the companion utility package
;;;
;;; Naming Convention:
;;; 1. "tss-" prefix for interactive command or customization options.
;;; 2. "tss--" prefix for ELisp programming API and variables.

(defvar-local tss--client nil
  "Reference to the `tss-client/class' for current buffer.")

(defun tss-setup-current-buffer ()
  "Setup TSS in current buffer"
  (interactive)
  (tss-manager/setup-buffer (current-buffer)))

(defun tss--active? ()
  "Check whether tss has been set up properly in the current buffer.

If the test fails, throw an error to prevent any other actions."
  (unless (tss-manager/aliveness-test (current-buffer))
    (error "TSS is not properly set up.")))


(defun tss--get-completions ()
  "Get a list of completions at current point in the current
buffer.

see `tss-client/get-completions' for details."
  (tss--active?)
  (let ((client tss--client)
        (cbuf (current-buffer)))
    (tss-client/set-buffer client cbuf)
    (tss-client/sync-buffer-content client)
    (tss-client/get-completions client)))
