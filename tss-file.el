;;; TSS for a single source file

(require 'tss-client)

(defclass tss-file/class (tss-client/class)
  ()
  "TSS client for single TypeScript source file.")

(defmethod tss-client/contains? ((this tss-file/class) file-buf)
  "Check whether THIS client is for FILE-BUF."
  (s-equals? (buffer-file-name file-buf)
             (buffer-file-name (oref this :buffer))))

(provide 'tss-file)
