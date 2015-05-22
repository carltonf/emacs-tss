(require 'ert)

(ert-deftest contains? ()
  (let ((client (tss-file/mocker)))
    (should (tss-client/contains? client
                                  (find-file-noselect "mockdata/single-file.ts")))
    (should-not (tss-client/contains? client (current-buffer)))))
