;;; TSS for a single source file

(require 'tss-client)

(defclass tss-file/class (tss-client)
  ()
  "TSS client for single TypeScript source file.")

;;#NO-TEST
(defun tss-file--init (file-buf)
  "Initialize FILE-BUF to prepare for TSS"
  t)
