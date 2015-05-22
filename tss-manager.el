;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Project Manager:
;;;
;;; Serve as the entry for `tss' for internal code, client manager and the glue
;;; between `tss-client' and `tss-comm'.
;;; 
;;; Responsibilities:
;;; - Manage all registered client types
;;; - Manage all existing clients
;;; - Identify&Create the right client type of new buffer.
;;; - Responsible for starting/stopping TSS service.

(require 'tss-project)
(require 'tss-file)
(require 'tss-comm)

(defvar tss-manager/client-list ()
  "A global list of all `tss-client'.")


(defvar tss-manager/registered-client-types '(tss-project/class
                                              tss-file/class)
  "A list of registered client class. The order of different
clients are predefined as it affects the type of client will be
used for a buffer.")

;;;#NO-TEST
(defun tss-manager/initialize ()
  "Initialize `tss-manager'"
  (warn "tss-manager/initialize: doing nothing yet."))

;;;#NO-TEST
(defun tss-manager/setup-buffer (file-buf)
  "Main entry for `tss-manager'. Setup TSS for FILE-BUF."
  (let ((client (tss-manager/client-loaded? file-buf)))
    (if client
        (tss-manager/configure-buffer client file-buf)
      (let* ((client-class (tss-manager/get-client-class file-buf))
             (client (make-instance client-class *initargs*)))
        (tss-client/connect client)))))

(defun tss-manager/client-loaded? (file-buf)
  "Check whether there is an alive client for FILE-BUF. Return
the client if found, o/w nil."
  (loop for client in tss-manager/client-list
        when (tss-client/contains? client file-buf)
        return client))


;;;#NO-TEST
(defun tss-manager--setup-project (project)
  (with-current-buffer project
    (unwind-protect
        (setq tss-client--proc (tss-comm--start-proc :type 'tsconfig
                                                      :client project))
      (unless tss-client--proc
        (error "Failed to start TSS for %s" project)))))

(defun tss-manager--setup-file (file-buf)
  (tss-file--init file-buf)
  (with-current-buffer file-buf
    (unwind-protect
        (setq tss-client--proc (tss-comm--start-proc :type 'file
                                                     :client file-buf))
      (unless tss-client--proc
        (error "Failed to start TSS for %s" file-buf)))))

;;;#NO-TEST
(defun tss-manager--get-project-create (file-buf)
  "Get the project object for FILE-BUF. In case no project can be
found, nil is returned."
  (let ((fpath (buffer-file-name file-buf)))
    (or (tss-manager--containing-project-loaded? fpath)
        ;; try to create and load new project
        (let ((prjroot (tss-project--locate-root fpath)))
          (when prjroot
            (tss-project--create prjroot))))))

;;;#NO-TEST
(defun tss-manager--activate-project (project)
  "Start TSS service for the project."

  (tss-manager--activate-project project)
  (add-to-list 'tss-manager--project-list project)
  project)


(provide 'tss-manager)
