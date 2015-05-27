;;; TSS Communication: code that talks with TSS

(defclass tss-comm/class ()
  ((client :type tss-client/class
           :initarg :client
           ;; TODO there is a bug in `:allow-nil-initform' in inheritance...
           ;; :initform nil
           :documentation "The client use this communication service.")
   ;; TODO we need spec for response format, refer to tsserver?
   (response :type (or list vector symbol)
             :initform nil
             :documentation "Response, well parsed and formatted.")
   (status :type symbol
           :initform :inactive
           :documentation
           "Current status of the communication: 
             :active everything is ok.
             :inactive communication is down."))
  :abstract t
  :documentation
  "Abstract base class for all TS Language service, e.g. tss, server and etc.
This class defines the interface between various services and `tss'.")

(defgeneric tss-comm/start ((class tss-comm/class))
  "Start the communication channel with file/project info in CLIENT.")

;;;TODO About sentinels, callbacks and status tracking, we know too little for
;;;now.
;;;#NO-TEST
(defmethod tss-comm/sentinel ((this tss-comm/class))
  "Invoke callbacks in CLIENT.COMM-SENTINELS.

Should be called whenever COMM status changes and before running
observer sentinels, various status info should be set, for now
only STATUS field is defined."
  (with-slots (client) this
    (loop for cb in (oref client comm-sentinels)
          do (funcall cb client))))

(defmethod tss-comm/destroy ((this tss-comm/class))
  "Destroy this communication, close up channels and free up
resources and etc.

Subclasses should call override and call this function in the
last."
  (oset this status :inactive))

(defgeneric tss-comm/command-inspect ((this tss-comm/class) cmd &optional cmdargs)
  "A method to send command and get raw response. Mainly a
development tool.

Subclasses should implement a Emacs interactive command to
display the response.")
