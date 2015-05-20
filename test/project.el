;;; Tests for `tss-project', prefix `tss-project--' is omitted.

(require 'ert)

(ert-deftest contains? ()
  (with-temp-buffer
    (let ((project (current-buffer)))
      (setq tss-project--root "/tmp/")
      (should (tss-project--contains? project
                                      "/tmp/some/my.ts"))
      (should-not (tss-project--contains? project
                                          "/home/my/test.ts"))
      ;; empty project root should issue an error
      (setq tss-project--root "")
      (should-error (tss-project--contains? project
                                            "/any/path/file.ts")))))


(ert-deftest new-project-id ()
  (should (equal (tss-project--new-project-id "myts")
                 " *TS: myts*"))
  (should-not (equal (tss-project--new-project-id "myts")
                     "TS: some")))

(ert-deftest get-proc ()
  (let ((mocked-proc "#<A mock proc>"))
    (should-not (with-temp-buffer
                  (setq tss-project--proc mocked-proc)
                  (tss-project--get-proc)))
    (should-not (with-temp-buffer
                  (setq tss--project "Not a buffer")
                  (setq tss-project--proc mocked-proc)
                  (tss-project--get-proc)))
    (should-not (let ((killed-buf (with-temp-buffer (current-buffer))))
                  (with-temp-buffer
                    (setq tss--project killed-buf)
                    (setq tss-project--proc mocked-proc)
                    (tss-project--get-proc))))
    (should (equal (with-temp-buffer
                     (setq tss--project (current-buffer))
                     (setq tss-project--proc mocked-proc)
                     (tss-project--get-proc))
                   mocked-proc))))
