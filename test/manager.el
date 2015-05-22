(require 'ert)

(load-file "helper.el")

(ert-deftest client-loaded? ()
  (let ((tss-manager/client-list (tss-manager/client-list-mocker)))
    (should (tss-manager/client-loaded? (find-file-noselect "mockdata/single-file.ts")))
    (should-not (tss-manager/client-loaded? (current-buffer)))))

(ert-deftest get-client-class ()
  (should (eq (tss-manager/get-client-class (find-file-noselect "mockdata/single-file.ts"))
              'tss-file/class)))
