(require 'ert)

(ert-deftest alist-path ()
  (let ((alist '((foo . ((bar . "llama")
                         (baz . "monkey"))))))
    (should (equal (cdr (assoc 'foo alist))
                   (tss-utils/alist-path alist '(foo))))
    (should (equal "llama"
                   (tss-utils/alist-path alist '(foo bar))))
    (should (equal "monkey"
                   (tss-utils/alist-path alist '(foo baz))))
    (should-not (tss-utils/alist-path alist '(foo nonexistent)))))
