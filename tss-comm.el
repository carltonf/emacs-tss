;;; TSS Communication: code that talks with TSS

(defclass tss-comm/class ()
  ((response :type string  ;TODO we need spec for the format, refer to tsserver?
             :initform ""
             :documentation "Response, well parsed and formatted."))
  :abstract t
  :documentation
  "Abstract base class for all TS Language service, e.g. tss, tserver and etc.
This class defines the interface between various services and `tss'.")

(defgeneric tss-comm/start ((class tss-comm/class) client)
  "Start the communication channel with file/project info in CLIENT.")
