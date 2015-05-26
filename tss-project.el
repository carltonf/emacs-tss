;;; TSS Project
;;;
;;; Only projects with a "tsconfig.json" as specified by the upstream are supported.
;;;
;;; TODO
;;; - [ ] Support arbitrary project. This requires a way to pass arbitrary parameters
;;; (including tsc parameters and files) to TSS.
;;; - [ ] Integrated with `projectile', which is a popular package supports a lot project-wide
;;; facilities.

; (fmakunbound 'tss-project/class)
(defclass tss-project/class (tss-client/class)
  ((type :type symbol
         :initform 'tsconfig))
  :documentation "TSS client class for tsconfig project.")

;;;#NO-TEST
(defmethod tss-client/applicable? :static ((class tss-project/class) file-buf)
  "Check whether a FILE-BUF belongs to a TSS project."
  (warn "Not implemented yet")
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Project
;;; The data structure for each project.
;;; - Technique: buffer as object, buffer-local vars as attributes.
;;; - Naming: `project' means project object

(defvar-local tss--project nil
  "TS project of the current buffer. Single reference for
TS project, all buffers using TSS should have this set.")

;;;: Project Fields
(defvar-local tss-project--name nil
  "Project name.")

(defvar-local tss-project--type nil
  "Project type, this can be:

'tsconfig: TS project with 'tsconfig.json.', for now this is the only feasible type.")

(defun tss-project--get-proc ()
  "Return the TSS process for current buffer. Nil if none is found."
  (cond
   ((or (null tss--project)
        (not (bufferp tss--project))
        (not (buffer-live-p tss--project)))
    nil)
   (t (buffer-local-value 'tss-project--proc tss--project))))

(defvar-local tss-project--root nil
  "Project root. Should be the same as `default-directory'.")

;;; end

(defconst tss-project--config-file "tsconfig.json"
  "TS project configuration file, see
https://github.com/Microsoft/TypeScript/wiki/tsconfig.json")

(defsubst tss-project--contains? (project fpath-abs)
  "Whether FPATH-ABS belong to a PROJECT."
  (let ((prjroot (buffer-local-value 'tss-project--root project)))
    (when (s-blank? prjroot)
      (error "%s: tss-project--root is NOT set." project))
    (s-prefix? prjroot fpath-abs)))



;;;#NO-TEST
(defsubst tss-project--locate-root (fpath)
  "Try to locate a project for FPATH. This function searches
upwards to find the nearest ancestor containing
`tss-project--config-file'."
  (locate-dominating-file fpath
                          tss--project-config-file))

(defconst tss-project--id-format " *TS: %s*"
  "ID format for project id.")

(defsubst tss-project--new-project-id (seed)
  "Return a project id, i.e. buffer name, for SEED.
SEED is usually a project name, but it can be arbitrary string
actually.

NOTE: this function does NOT guarantee the uniqueness of the project."
  (format tss-project--id-format seed))

;;;#NO-TEST
(defun tss-project--create (prjroot)
  "Create a project object for PRJROOT."
  (let* ((name (f-filename prjroot))
         (project (generate-new-buffer
                   (tss-project--new-project-id name))))
    (with-current-buffer project
      (setq tss-project--name name
            default-directory prjroot
            tss-project--root prjroot
            tss--project project))))

;; TODO let TYPE also be able to figure out the TSS parameters
;; TODO .git or package.json types can be supported by reading package.json and
;; include all TS sources.
(defun* tss-project--configure (&key name prjroot root-sources type)
  "Set PRJROOT and ROOT-SOURCES for NAME in
`tss--project-root-sources-table'. Return project NAME.

Always use this function to setup TS projects.
`tss--project-config-file' uses this function to configure
project.

If PRJROOT is nil or 'current, then use the directory of the
configuration file.

ROOT-SOURCES is a list of TS source files. If ROOT-SOURCES is
nil, try to figure out root sources using TYPE.

TYPE is a way to automatically figure out root-sources, the
following types are supported: 

'tsconfig: Project configuration \"tsconfig.json\" is present, no
need for Emacs to set up any configuration for TSS.

'gulp: assume tools/typings/typescriptApp.d.ts is the definition
TS reference files for this project. All other \".d.ts\" files
will also be included. This setup is popularized by 
http://weblogs.asp.net/dwahlin/creating-a-typescript-workflow-with-gulp
https://github.com/DanWahlin/AngularIn20TypeScript

'any or nil: in this case, ROOT-SOURCES has been configured, o/w
an error is thrown.

ROOT-SOURCES can be relative to PRJROOT, which itself needs to be
+absolute (TODO eliminate this limit)."
  (setq prjroot (pcase prjroot
                  ((or `nil `current)
                   (if load-file-name
                       ;; loaded by script
                       (file-name-directory load-file-name)
                     ;; evaluate directly
                     (expand-file-name default-directory)))
                  ((pred file-exists-p)
                   (expand-file-name prjroot))
                  (_ (error "'%s' is NOT a valid project root." prjroot))))

  (setq root-sources
        (or root-sources
            (pcase type
              (`tsconfig
               t)
              (`gulp
               (directory-files (expand-file-name (f-join "tools" "typings") prjroot)
                                t "\\.d\\.ts$"))
              ((or `nil `any)
               (unless (listp root-sources)
                 (error "For %s-type TS project, root-sources have to be set!"
                        type))))))

  (puthash name (list prjroot
                      ;; root sources
                      (or (eq root-sources t)
                          (loop for path in root-sources
                                collect (expand-file-name path prjroot))))
           tss--project-root-sources-table)
  name)
