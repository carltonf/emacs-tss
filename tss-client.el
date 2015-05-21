;;;: Generic TSS Client
;;; Various types of project and file are all subclass of TSS client
;;; Here common attributes and methods are defined.
;;;

(require 'eieio)

(defvar-local tss-client nil
  "Reference to an `tss-client' object.")

(defclass tss-client/class ()
  ((proc :type process
         :documentation "Current TSS process for the client.")
   ;; communication part
   (server-response :type string
                    :documentation "*Complete* TSS response.")
   (incomplete-server-response :type string
                               :documentation "Incomplete/intermediate TSS response.")
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
(defgeneric tss-client/contains? ((this tss-client/class) file-buf)
  "Check whether THIS contains FILE-BUF.")

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
