(ert-deftest response-balanced? ()
  (let ((str "some") stag etag)
    (should (tss-tst/response-balanced? str stag etag)))
  (let ((str "{\"isMemberCompletion\":false,\"isNewIdentifierLocation\":true,\"entries\":[{\"name\":\"greeter\",\"kind\":\"var\",\"kindModifiers\":\"\",\"type\":\"var greeter: Greeter\",\"docComment\":\"\"}],\"prefix\":\"greeter\"}")
        (stag "{") (etag "}"))
    (should (tss-tst/response-balanced? str stag etag)))
  (let ((str "{\"isMemberCompletion\":false,\"isNewIdentifierLocation\":true,\"entries\":[{\"name\":\"greeter\",\"kind\":\"var\",\"kindModifiers\":\"\",\"type\":\"var greeter: Greeter\",\"doc")
        (stag "{") (etag "}"))
    (should-not (tss-tst/response-balanced? str stag etag))))


(ert-deftest receive-response:normal ()
  (let ((proc "#<mocked-proc, should NOT be used>")
        (res "mocked message from server, should be set before use.")
        (tst "#<mocked tss-tst/class object, recreate it before each test>"))
    (progn (setq tst (make-instance tss-tst/class)
                 res "\"loaded /home/carl/try/typescript.ts, TSS listening..\"")
           (tss-tst/receive-response tst proc res)
           (should (equal 'succeed
                          (oref tst response))))
    (progn (setq tst (make-instance tss-tst/class)
                 res "null")
           (tss-tst/receive-response tst proc res)
           (should (equal 'null
                          (oref tst response))))
    (progn (setq tst (make-instance tss-tst/class)
                 res "{\"kind\":\"var\",\"kindModifiers\":\"\",\"textSpan\":{\"start\":195,\"length\":7},\"documentation\":[],\"type\":\"var greeter: Greeter\",\"docComment\":\"\"}")
           (oset tst response-start-tag "{")
           (oset tst response-end-tag "}")
           (tss-tst/receive-response tst proc res)
           (should (equal '((docComment . "")
                            (type . "var greeter: Greeter")
                            (documentation . [])
                            (textSpan
                             (length . 7)
                             (start . 195))
                            (kindModifiers . "")
                            (kind . "var"))
                          (oref tst response))))
    (progn (setq tst (make-instance tss-tst/class)
                 res "[\"/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/lib.d.ts\",\"/home/carl/try/typescript.ts\"]")
           (oset tst response-start-tag "[")
           (oset tst response-end-tag "]")
           (tss-tst/receive-response tst proc res)
           (should (equal ["/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/lib.d.ts" "/home/carl/try/typescript.ts"]
                          (oref tst response))))))

(ert-deftest receive-response:incomplete ()
  (let ((proc "#<mocked-proc, should NOT be used>")
        (res "mocked message from server, should be set before use.")
        (tst "#<mocked tss-tst/class object, recreate it before each test>"))
    (progn (setq tst (make-instance tss-tst/class)
                 res "[\"/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/")
           (oset tst response-start-tag "[")
           (oset tst response-end-tag "]")
           (tss-tst/receive-response tst proc res)
           (should (equal nil (oref tst response)))
           (should (equal res (oref tst incomplete-response)))
           (setq res "lib.d.ts\",\"/home/carl/try/typescript.ts\"]")
           (tss-tst/receive-response tst proc res)
           (should (equal ["/home/carl/.nvm/versions/io.js/v1.6.4/lib/node_modules/typescript-tools/node_modules/typescript/bin/lib.d.ts" "/home/carl/try/typescript.ts"]
                          (oref tst response)))
           (should (equal "" (oref tst incomplete-response))))))

(load-file "helper.el")

(ert-deftest get-cmdstr ()
  (let* ((client (tss-file/mocker))
         (tst (make-instance tss-tst/class :client client)))
    (should (string-match-p "^[^ \t]+tss [^ \t]+mockdata.single-file\\.ts$"
                            (tss-tst/get-cmdstr tst))))
  (let* ((client (make-instance tss-project/class))
         (tst (make-instance tss-tst/class :client client)))
    (should (string-match-p "^[^ \t]+tss$" 
                            (tss-tst/get-cmdstr tst)))))

(ert-deftest get-posarg ()
  (with-temp-buffer
    (loop for num in (number-sequence 1 100)
          do (insert (format "%4d" num))
          when (zerop (% num 10))
          do (insert "\n"))
    (should (equal "11 1"
                   (tss-tst/get-posarg (current-buffer))))
    (save-excursion
      (search-backward "45")
      (should (equal "5 19"
                     (tss-tst/get-posarg (current-buffer)))))
    (save-excursion
      (goto-char (point-min))
      (should (equal "1 1"
                     (tss-tst/get-posarg (current-buffer)))))))
