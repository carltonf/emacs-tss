(require 'ert)

(ert-deftest assoc-path ()
  (let ((alist '((foo . ((bar . "llama")
                         (baz . "monkey"))))))
    (should (equal (cdr (assoc 'foo alist))
                   (tss-utils/assoc-path alist '(foo))))
    (should (equal (cdr (assoc 'foo alist))
                   (tss-utils/assoc-path alist 'foo)))
    (should (equal "llama"
                   (tss-utils/assoc-path alist '(foo bar))))
    (should (equal "monkey"
                   (tss-utils/assoc-path alist '(foo baz))))
    (should-not (tss-utils/assoc-path alist '(foo nonexistent)))))
