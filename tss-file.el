;;; TSS for a single source file

(require 'tss-client)

(defclass tss-file/class (tss-client/class)
  ((type :type symbol
         :initform 'file))
  :documentation
  "TSS client for single TypeScript source file.")

;;;: Static Methods
(defmethod tss-client/applicable? :static ((class tss-file/class) file-buf)
  "`tss-file' is always applicable, but this should not be used
before checks for other types of clients."
  t)

;;;: Object Methods
;;;#+NO-TEST
(defmethod tss-client/initialize ((this tss-file/class))
  (with-slots (name buffer initp) this
    (unless (s-present? name)
      (setq name (f-filename (buffer-file-name buffer))))
    ;; after everything has been properly setup
    (setq initp t)))

(defmethod tss-client/contains? ((this tss-file/class) file-buf)
  "Check whether THIS client is for FILE-BUF."
  (s-equals? (buffer-file-name file-buf)
             (buffer-file-name (oref this :buffer))))


;;;#NO-TEST
(defmethod tss-client/active? ((this tss-file/class))
  (with-slots (initp buffer comm) this
   (and initp
        (and (bufferp buffer)
             (buffer-live-p buffer))
        (and (tss-comm/class-child-p comm)
             (tss-comm/alive? comm)))))

;;;#NO-TEST
(defmethod tss-client/connect ((this tss-file/class) service)
  "A new instance of TSS service always starts for `tss-file/class'"
  (with-slots (comm) this
    (setq comm service)
    (tss-comm/start comm)))

;;;#NO-TEST
(defmethod tss-client/destory ((this tss-file/class))
  (with-slots (comm buffer initp) this
    (setq initp nil)
    (tss-comm/destroy comm)
    (with-current-buffer buffer
      (setq tss--client nil))))

(provide 'tss-file)
