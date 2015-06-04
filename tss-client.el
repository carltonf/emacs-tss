;;;: Generic TSS Client
;;;
;;; Abstract internal representation of a unit of TSS client.
;;; Various types of project and file are all subclass of TSS client
;;;
;;; Life Cycle of a TSS client, managed by `tss-manager'
;;; - Visiting a new file can lead to creation of a new client
;;; - All "active" clients are enlisted in `tss-manager/client-list'.
;;; - When `tss-client/active?' return false, `tss-client/destroy' should be
;;;   called to clean up.
;;; - Upon fatal errors or user request, `tss-manager' can destroy a client.

(require 'eieio)

;; (fmakunbound 'tss-client/class)
(defclass tss-client/class ()
  ((name :initarg :name
         :initform ""
         :type string
         :documentation "Client name. If not set, a name will be derived from `buffer'.")
   (buffer :initarg :buffer
           :type buffer
           :documentation "`current-buffer' of the client. Need
           to be updated for methods that need to access buffer
           info.")
   (job-queue :type tss-client-job/class-list
              :initform nil
              :documentation "The job queue. Each job is an assoc
              list. Used for async functionality.")
   (comm :type tss-comm/class
         :documentation "Current TSS communication object for the client.")
   (comm-sentinels :type list
                   :initform nil
                   :documentation
                   "A list of callback functions that get called
                   when COMM status changed. Callbacks will be
                   passed with THIS object.")
   (type :type symbol
         :initform nil
         :documentation "TS file/project types, currently only 'file and 'tsconfig.")
   (cmd-cache :type hash-table
              :initform (let ((cache (make-hash-table :test #'equal
                                                      :size 8)))
                          (puthash :errors nil cache)
                          (puthash :doc "" cache)
                          (puthash :completions nil cache)
                          (puthash :connect nil cache)
                          cache)
              :documentation
              "Commands cache, for performance optimization and
              async functionality. Whether a cache is valid
              depends on each client's internal mechanism,
              usually it's recorded time stamps or tick counters
              like `buffer-modified-tick'.

              Supported commands:
              :errors, an error list, see `tss--get-errors'.
              :doc, documentation string, see `tss--get-doc-at-point'.
              :completions, a list of completions, see `tss--get-completions'.")
   ;; internal status for life cycle
   (initp :type boolean
          :initform nil
          :documentation "Set by constructor to indicate a properly initialized object."))
  :abstract t
  :documentation
  "Abstract base class for all TSS clients, e.g. files, various
  project types and etc. This is the interface `tss-manager' will
  see.")

(defclass tss-client-job/class ()
  ((cmd :type symbol
        :initarg :cmd
        :documentation "see `cmd-cache' field in `tss-client/class'".)
   (timestamp :type list
              :initform (current-time))
   (extras :type list
           :initform nil
           :documentation
           "Arbitrary params set up by `tss--comm/class'."))
  "A job for `tss-client/class', maintained by `job-queue' field
in `tss-client/class'.")

(defmethod tss-client/job-enqueue ((this tss-client/class) job)
  "Enqueue JOB."
  (with-slots (job-queue) this
    (if (null job-queue)
        (setq job-queue (list job))
      (setcdr (last (oref this job-queue)) (list job)))))

(defmethod tss-client/job-dequeue ((this tss-client/class))
  "Dequeue a job."
  (with-slots (job-queue) this
    (setq job-queue (cdr job-queue))))

;;;: Static Methods
;; (fmakunbound 'tss-client/applicable?)
(defgeneric tss-client/applicable? ((class tss-client/class) file-buf)
  "A STATIC method. Check whether client type CLASS is applicable
to FILE-BUF. In case of a project, this is just whether the file
is contained by this project.")

;;;: Object Methods
(defgeneric tss-client/initialize ((this tss-client/class))
  "Initialize objects of `tss-client/class', should be called
before any use of the objects.")

(defgeneric tss-client/contains? ((this tss-client/class) file-buf)
  "Check whether THIS client contains FILE-BUF.")

(defgeneric tss-client/connect ((this tss-client/class) service)
  "Connect to TSS.")

(defgeneric tss-client/active? ((this tss-client/class))
  "Check whether THIS client is still active. If not, usually a
`tss-client/destory' call is followed.")

(defgeneric tss-client/destory ((this tss-client/class))
  "Destroy THIS, clean up and free resources. In particular,
configurations done to buffer in `tss-client/configure-buffer'
should be undone.")

;;;#NO-TEST
(defmethod tss-client/configure-buffer ((this tss-client/class) buffer)
  "Configure BUFFER with regards to THIS client.

If subclasses override this function and they should call this
function in the last."
  (with-current-buffer buffer
    (setq tss--client this)))

(defconst tss-client/supported-cmds
  '(:update :doc :completions :errors :connect)
  "A list of commands supported by `tss-client'. All
`tss-comm/class' have to implement these.")

(defun tss-client/command-inspect (cmd)
  "Inspect communication for THIS client. CMD is one of
`tss-client/supported-cmds'.

This method is internal and serves development purpose mostly.

As a development tool, abstraction layer is not well contained,
e.g. `buffer' is usually not used by `tss-comm' but here
`tss-comm/command-inspect' will extract info directly from
buffer."
  (interactive (list
                (progn
                  ;; TODO we need to check the status of `tss-client'
                  (unless tss--client
                    (error "TSS: No active tss--client found."))
                  (completing-read "TS Command: "
                                   tss-client/supported-cmds
                                   nil t))))
  (let ((cmd (intern cmd))
        (client tss--client))
    (tss-client/set-buffer client)
    (tss-client/sync-sources client)

    (tss-comm/command-inspect (oref client comm) cmd)
    (pp-display-expression (gethash cmd (oref client cmd-cache))
                           "*TSS CMD Inspect*")))

;;;: Set of supported tss-client API
;;;
;;; API for 3rd ELisp library to utilize TSS. See notes below for usage
;;; cautions.
;;;
;;; A normal flow of using the APIs:
;;; 0. use `tss--active?' which calls `tss-client/active?' and some others
;;; 1. `tss-client/set-buffer' set up buffer.
;;; 2. `tss-client/sync-sources' sync/update source.
;;; 3. Prepare needed params, usually the default is sufficient.
;;; 4. Call specific `tss-client' API.
;;; 5. Retrieve response, extract info. Handle errors if any.
;;;
;;;
;;; Usually 3rd party should use more API wrappers offered in `tss.el'. Only
;;; when they need extra flexibility, the `tss-client' API is used.
;;;
;;; Notes on the two API sets: `tss.el' and `tss-client.el'
;;; 1. `tss.el' expose the conventional ELisp style API: no need to pass EIEIO object.
;;; 2. `tss.el' only deal with the most common use cases: the current buffer,
;;; the current point and etc. AND there is no need to go through the "normal"
;;; flow documented above.
;;; 3. `tss-client.el' tries to attain maximum flexibility.
;;; 4. `tss.el' is considered more stable than `tss-client.el', as the latter
;;; needs to adjust for new use cases more often
;;;
;;;
;;;

;;;#NO-TEST
(defgeneric tss-client/source-changed? ((this tss-client/class))
  "Check whether source has been changed. Called before any real
work done in the various commands.

Due to the varieties of client types, each client should
implement this function.")

;;;#NO-TEST
(defmethod tss-client/set-buffer ((this tss-client/class) &optional buffer)
  "Update :buffer to BUFFER or `current-buffer' if BUFFER is nil.
Needed as various commands can only be done with regards to the
`current-buffer'."
  (let ((buf (or buffer (current-buffer))) )
    (when (tss-client/contains? this buf)
      (oset this :buffer buf))))

;;; TODO non-optimized and not well-scaled.
;;; 1. Projects have multiple buffers not a single buffer
;;; 2. Use timestamps to only sync updated buffers.
;;; 3. Make the return value indicate whether a sync has happened, some methods
;;; are time-consuming and should not run if no content updates.
;;; 4. Change the name and delegate the work to client.
;;;#NO-TEST
(defmethod tss-client/sync-sources ((this tss-client/class)
                                    &optional source linecount)
  "Sync buffer content with ts service. By default file path, content,
line count are retrieved from `current-buffer'. Return t if sync
is done, nil o/w.

Optional arguments SOURCE, LINECOUNT are supplied to allow extra
flexibility in updating the source. These are useful when you are
doing completing/templating when the needed changes are not even
in buffer yet, but you still want to get some info about these
supposed changes (like definition/quickInfo and etc.).

Syncing content is `tss-client/class's responsibility. If needed,
client should call this method before issuing commands to `comm'.

TODO I think these are needed because the ts service only support
stateless queries."
  (if (tss-client/source-changed? this)
      (with-slots (comm job-queue buffer) this
        (tss-client/job-enqueue this (make-instance tss-client-job/class
                                                    :cmd :errors))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (let* ((source (or source
                               (buffer-substring-no-properties (point-min) (point-max))))
                   (linecount (or linecount
                                  (count-lines (point-min) (point-max)))))
              (tss-comm/update-source comm source linecount (buffer-file-name))))
          t))
    nil))

;;;#NO-TEST
(defmethod tss-client/get-completions ((this tss-client/class)
                                       &optional line column)
  "Get completions at point (LINE COLUMN) or current point. A
list of completions should only contain basic info, there should
be NO detailed info."
  (if (tss-client/source-changed? this)
      (with-current-buffer (oref this :buffer)
        (save-restriction
          (widen)
          (let* ((line (or line (line-number-at-pos)))
                 (column (or column (current-column)))
                 (res (tss-comm/get-completions (oref this comm)
                                                line column (buffer-file-name))))
            (puthash :completions res (oref this cmd-cache)))))
    (gethash :completions (oref this cmd-cache))))

;;;#NO-TEST
(defmethod tss-client/get-doc ((this tss-client/class)
                               &optional line column)
  "Get documentation on thing at (LINE COLUMN) or current point."
  (if (tss-client/source-changed? this)
      (with-current-buffer (oref this :buffer)
        (save-restriction
          (widen)
          (let* ((line (or line (line-number-at-pos)))
                 (column (or column (current-column)))
                 (res (tss-comm/get-doc (oref this comm)
                                        line column (buffer-file-name))))
            (puthash :doc res (oref this cmd-cache)))))
    (gethash :doc (oref this cmd-cache))))

;;; TODO incomplete, need error caching, actually we need general API caching to
;;; optimize and implement async functionality.
(defmethod tss-client/get-errors ((this tss-client/class))
  "Retrieve errors about THIS client."
  (if (tss-client/source-changed? this)
      (let ((res (tss-comm/get-errors (oref this comm))))
        (puthash :errors res (oref this cmd-cache)))
    (gethash :errors res (oref this cmd-cache))))

(defmethod tss-client/request-errors-bg ()
  "Request errors in the background. Return immediately."
  (if (tss-client/source-changed? this)
      (tss-comm/send-errors-cmd (oref this comm))
    nil))

(provide 'tss-client)
