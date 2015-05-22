;;; TSS for a single source file

(require 'tss-client)

(defclass tss-file/class (tss-client/class)
  ((type :type symbol
         :initform 'file
         :documentation "TS file type."))
  "TSS client for single TypeScript source file.")

;;;: Static Methods
(defmethod tss-client/applicable? :static ((class tss-file/class) file-buf)
  "`tss-file' is always applicable, but this should not be used
before checks for other types of clients."
  t)

;;;: Object Methods
(defmethod tss-client/contructor ((this tss-file/class)))

(defmethod tss-client/contains? ((this tss-file/class) file-buf)
  "Check whether THIS client is for FILE-BUF."
  (s-equals? (buffer-file-name file-buf)
             (buffer-file-name (oref this :buffer))))


;;;#NO-TEST
(defmethod tss-client/connect ((this tss-file/class))
  "A new instance of TSS service always starts for `tss-file/class'"
  (with-slots (proc) this
   (setq proc (tss-comm/start this))))

(provide 'tss-file)
