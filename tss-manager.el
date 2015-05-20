;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Project Manager:  for project and file
;;; - Manage all existing projects
;;; - Responsible for starting/stopping TSS service.

(require 'tss-project)
(require 'tss-file)
(require 'tss-comm)

(defvar tss-manager--project-list '()
  "A list of project objects, i.e. project buffers.")

(defsubst tss-manager--containing-project-loaded? (fpath)
  "Check whether a loaded project in `tss-manager--project-list'
contains FPATH, if so returns this project."
  (loop for p in tss-manager--project-list
        when (tss-project--contains? p fpath)
        return p))

;;;#NO-TEST
(defun tss-manager--setup-buffer (file-buf)
  "Main entry for `tss-manager'. Setup TSS for FILE-BUF."
  (let ((project (tss-manager--get-project-create file-buf)))
    (if project
        (tss-manager--setup-project project)
      (tss-manager--setup-file file-buf))))

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
