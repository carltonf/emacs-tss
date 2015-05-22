(require 'ert)

(load-file "helper.el")

(ert-deftest client-loaded? ()
  (let ((tss-manager/client-list (tss-manager/client-list-mocker)))
    (should (tss-manager/client-loaded? (find-file-noselect "mockdata/single-file.ts")))
    (should-not (tss-manager/client-loaded? (current-buffer)))))
