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
  "Check whether tss has been set up properly in the current
buffer."
  (tss-manager/aliveness-test (current-buffer)))

(defun tss--active-test ()
  "Same as `tss--active?' but if the test fails, throw an error
to prevent any other actions."
  (if (tss--active?)
      t
    (error "TSS is not properly set up.")))

;;; TODO The format of returned result is not well defined, as we need to know
;;; more about typescript spec to finalize them. For now the format is basically
;;; the one returned by the `typescript-tools'.

(defun tss--get-completions ()
  "Get a list of completions at current point in the current
buffer. nil if none is found.

  Example of returned result:
  ((entries .
            [((kindModifiers . <|public|private|...>)
              (kind . <property|...>)
              (name . <name>))
             |...])
   (isNewIdentifierLocation . :json-false)
   (isMemberCompletion . t))

See `tss-client/get-completions' for details."
  (tss--active-test)
  (let ((client tss--client)
        (cbuf (current-buffer)))
    (tss-client/set-buffer client cbuf)
    (tss-client/sync-buffer-content client)
    (tss-client/get-completions client)))

(defun tss--get-doc-at-point ()
  "Get documentation on thing at point.

Example of returned result:
  ((docComment . <string>)
   (type . <full string of declaration>)
   (documentation . <array of something?>)
   (textSpan
    (length . 17)
    (start . 216))
   (kindModifiers . <declare|...>)
   (kind . <kind>))

See `tss-client/get-doc'"
  (tss--active-test)
  (let ((client tss--client)
        (cbuf (current-buffer)))
    (tss-client/set-buffer client cbuf)
    (tss-client/sync-buffer-content client)
    (tss-client/get-doc client)))

(defun tss--get-errors ()
  "Get a list of errors on `tss--client'.

NOTE this is not only errors for current buffer.

Example of returned result:
  [((category . \"Error\")
    (phase . \"Semantics\")
    (code . 2322)
    (text . \"Type 'string' is not assignable to type 'number'.\")
    (end
     (character . 10)
     (line . 1))
    (start
     (character . 5)
     (line . 1))
    (file . <file-path>))
   ((category . \"Error\")
    (phase . \"Semantics\")
    (code . 2322)
    (text . \"Type 'number' is not assignable to type 'string'.\")
    (end
     (character . 10)
     (line . 12))
    (start
     (character . 5)
     (line . 12))
    (file . <file-path>))]. "
  (tss--active-test)
  (let ((client tss--client)
        (cbuf (current-buffer)))
    (tss-client/set-buffer client cbuf)
    (tss-client/sync-buffer-content client)
    (tss-client/get-errors client)))

(defun tss--request-errors-bg ()
  "Request errors info in the background. Unlike
`tss--get-errors', this function returns immediately. Use
`tss--requested-errors' to retrieve the result.

Getting errors seem to be time consuming, so an async function is
useful if the client should not block."
  (tss--active-test)
  (error "Not implemented."))

(defun tss--requested-errors ()
  "Return the errors requested beforehand with
`tss--request-errors-bg'. If error info is ready, return it.
Otherwise return nil. To guarantee getting results, the client
code can do the busy polling."
  (tss--active-test)
  (error "Not implemented."))
