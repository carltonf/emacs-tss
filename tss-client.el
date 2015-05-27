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

(defvar-local tss-client nil
  "Reference to an `tss-client' object.")

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
   ;; internal status for life cycle
   (initp :type boolean
          :initform nil
          :documentation "Set by constructor to indicate a properly initialized object.")
   ;; TODO still needed?
   (last-send-string-failed-p :documentation "TODO seems to be a indicator")
   (current-active-p :documentation "TODO whether TSS has been setup"))
  :abstract t
  :documentation
  "Abstract base class for all TSS clients, e.g. files, various
  project types and etc. This is the interface `tss-manager' will
  see.")

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

(defgeneric tss-client/connect ((this tss-client/class) file-buf)
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
    (setq tss-client this)))

;;;#NO-TEST
(defmethod tss-client/update-buffer ((this tss-client/class))
  "Update :buffer to `current-buffer'. Needed as various commands
can only be done with regards to the `current-buffer'."
  (when (tss-client/contains? this (current-buffer))
    (oset this :buffer (current-buffer))))

;;;#NO-TEST
(defmethod tss-client/comm-inspect ((this tss-client/class) comm-cmds)
  "Inspect communication for THIS client. COMM-CMDS is a list and
can be anything that the underlying communication understands.
This method is internal and serves development purpose mostly.

NOTE: Communication inspection is communication specific, so the
entry interactive command is defined per communication type.
However no communication can be conducted without some preps or
info supplied by `tss-client/class', so we internally need this
method `tss-client/comm-inspect' as the inspection entry which
later delegates real work to specific communication methods."
  (tss-client/sync-buffer-content this)
  (tss-comm/command-inspect (oref this comm) comm-cmds))

;;;: Set of supported tss API
;;;
;;; API for 3rd ELisp library to utilize TSS.
;;; 
;;; Usually 3rd party should not use these directly, they should use more
;;; conventional API wrappers offered in `tss.el'.
;;;#NO-TEST
(defmethod tss-client/sync-buffer-content ((this tss-client/class)
                                           &optional source linecount path)
  "Sync buffer content with ts service. By default file path, content,
line count are retrieved from `current-buffer'.

Optional arguments SOURCE, LINECOUNT, PATH are supplied to allow
extra flexibility in updating the source. These are useful when
you are doing completing/templating when the needed changes are
not even in buffer yet, but you still want to get some info about
these supposed changes (like definition/quickInfo and etc.).

Syncing content is `tss-client/class's responsibility. If needed,
client should call this method before issuing commands to `comm'.

TODO I think these are needed because the ts service only support
stateless queries."
  (tss-client/update-buffer this)
  (with-current-buffer (oref this buffer)
    (save-restriction
      (widen)
      (let* ((source (or source
                         (buffer-substring-no-properties (point-min) (point-max))))
             (linecount (or linecount
                            (count-lines (point-min) (point-max))))
             (path (or path (buffer-file-name))))
        (tss-comm/update-source (oref this comm) source linecount path)))))

(provide 'tss-client)
