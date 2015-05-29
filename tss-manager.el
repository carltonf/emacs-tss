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

(require 'tss-client)
(require 'tss-file)
(require 'tss-project)

(require 'tss-comm)

(defvar tss-manager/client-list ()
  "A global list of all `tss-client'.")

;;;#NO-TEST
(defun tss-manager/clean-all-clients ()
  "Helper command to remove all clients, delete all
communications."
  (interactive)
  (loop for client in tss-manager/client-list
        do (tss-client/destory client))
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
    (tss-client/configure-buffer client file-buf)))

(defun tss-manager/client-loaded? (file-buf)
  "Check whether there is an alive client for FILE-BUF. Return
the client if found, o/w nil."
  (loop for client in tss-manager/client-list
        when (tss-client/contains? client file-buf)
        return client))

(defun tss-manager/get-client-class (file-buf)
  "Get the client class that is applicable to FILE-BUF.
See `tss-manager/registered-client-classes' for all possible
classes. Return nil if no class can be used."
  (loop for class in tss-manager/registered-client-classes
        when (tss-client/applicable? class file-buf)
        return class))

;;; TODO better report on which part fails the test
;;;#NO-TEST
(defun tss-manager/aliveness-test (buffer)
  "Check whether TSS can function normal in BUFFER.

Aliveness test should be done before using TSS API, this test is
conducted following a chain:
1. global status test at manager level
2. local status, `tss--client' should be set in BUFFER
3. Then `tss--client' -> `tss-comm'."
  ;; global status checking
  (and (and
        tss-manager/client-list
        (tss-client/class-list-p tss-manager/client-list))
       ;; local status checking
       (with-current-buffer buffer
         (and tss--client
              (tss-client/active? tss--client)))))


(provide 'tss-manager)
