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

(defclass tss-client/class ()
  ((buffer :initarg :buffer
           :initform nil
           :type buffer
           :documentation "Start buffer passed in to initialize new tss-client objects.")
   (comm :type tss-comm/class
         :initform nil
         :documentation "Current TSS communication object for the client.")
   (type :type symbol
         :initform nil
         :documentation "TS file/project types, currently only 'file and 'tsconfig.")
   ;; internal status for life cycle
   (initp :type boolean
          :initform nil
          :documentation "Set by constructor to indicate a properly initialized object.")
   ;; communication part
   
   ;; TODO still needed?
   (last-send-string-failed-p :documentation "TODO seems to be a indicator")
   (current-active-p :documentation "TODO whether TSS has been setup"))
  :allow-nil-initform t
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
  "Check whether THIS is still active. If not, usually
a `tss-client/destory' call is followed.")

(defgeneric tss-client/destory ((this tss-client/class))
  "Destroy THIS, clean up and free resources.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO the following two are bad naming, which I believe is a problem with TSS.
;; TODO the official tsserver has sanitized JSON response
(defvar-local tss-client--json-response-start-char ""
  "Start character of response string, paired with `tss--json-response-end-char'.

WARNING: TSS response is NOT JSON actually, it's more like
JavaScript data get inspected. For now, there are string, array
and object. So here we need to set the start&end char to know
what responses we are receiving.")

(defvar-local tss-client--json-response-end-char ""
  "End character of response string.
See `tss--json-response-start-char' for more info.")

;; }}


(provide 'tss-client)
