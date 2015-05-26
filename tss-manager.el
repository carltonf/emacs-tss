;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Project Manager:
;;;
;;; Serve as the entry for `tss' for internal code, client manager and the glue
;;; between `tss-client' and `tss-comm'.
;;; 
;;; Responsibilities:
;;; - Manage all registered client types
;;; - Manage all existing clients and their life cycle, see `tss-client' for detail.
;;; - Identify&Create the right client type of new buffer.
;;; - Responsible for starting/stopping TSS service.

(require 'tss-project)
(require 'tss-file)
(require 'tss-comm)

(defvar tss-manager/client-list ()
  "A global list of all `tss-client'.")

;;; TODO too specific, delegate to client.destroy
;;;#NO-TEST
(defun tss-manager/clean-all-clients ()
  "Helper command to remove all clients, delete all
communications."
  (interactive)
  (loop for proc in (process-list)
      when (s-starts-with? "tss-" (process-name proc))
      do (delete-process proc))
  (setq tss-manager/client-list nil))

(defvar tss-manager/registered-client-classes '(tss-project/class
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
  (let ((client (tss-manager/client-loaded? file-buf))
        service)
    (unless client
      (let* ((client-class (tss-manager/get-client-class file-buf)))
        (setq client (make-instance client-class :buffer file-buf)
              ;; TODO need options to set what service to use
              service (make-instance tss-tst/class :client client))
        (tss-client/initialize client)
        (tss-client/connect client service)
        (add-to-list 'tss-manager/client-list client)))
    (tss-manager/configure-buffer client file-buf)))

(defun tss-manager/client-loaded? (file-buf)
  "Check whether there is an alive client for FILE-BUF. Return
the client if found, o/w nil."
  (loop for client in tss-manager/client-list
        when (tss-client/contains? client file-buf)
        return client))

;;;#NO-TEST
(defun tss-manager/configure-buffer (client file-buf)
  "Configure FILE-BUF with CLIENT"
  (with-current-buffer file-buf
    (setq tss-client client)))

(defun tss-manager/get-client-class (file-buf)
  "Get the client class that is applicable to FILE-BUF.
See `tss-manager/registered-client-classes' for all possible
classes. Return nil if no class can be used."
  (loop for class in tss-manager/registered-client-classes
        when (tss-client/applicable? class file-buf)
        return class))

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
