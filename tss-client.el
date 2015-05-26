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
           :documentation "Start buffer passed in to initialize
           new tss-client objects. Might NOT be useful after the
           initialization.")
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

(provide 'tss-client)
