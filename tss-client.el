;;;: Generic TSS Client
;;; Various types of project and file are all subclass of TSS client
;;; Here common attributes and methods are defined.
;;;

(defvar-local tss-client--proc nil
  "Current TSS process for project.")

;;; local variables for parsing/storing TSS service messages {{
;;
;; TODO using EIEIO, we can create a new object to hold these data instead of a
;; flat project object.
(defvar-local tss-client--last-send-string-failed-p nil)

(defvar-local tss-client--current-active-p t)

(defvar-local tss-client--server-response nil
  "Main variable for storing TSS response. Though this is
buffer-local var, for project files only the ones set in project
buffer are meaningful.")

(defvar-local tss-client--incomplete-server-response ""
  "Intermediate response content. see `tss-client--server-response' for
more info.")

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
