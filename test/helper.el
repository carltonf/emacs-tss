(defun* tss-file/mocker ()
  "Mock a `tss-file' object for testing."
  (let* ((buffer (find-file-noselect "mockdata/single-file.ts"))
         (client (make-instance tss-file/class
                                :buffer buffer)))
    client))

(defun* tss-manager/client-list-mocker ()
  "Mock a `tss-manager/client-list', use `let' binding to use."
  (let ((clist (list (tss-file/mocker))))
    clist))
