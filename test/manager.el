(require 'ert)

(ert-deftest containing-project-loaded ()
  (let ((tss-manager--project-list
         (list (generate-new-buffer " *temp*")
               (generate-new-buffer " *temp*"))))
    (unwind-protect
        (progn
          (with-current-buffer (nth 0 tss-manager--project-list)
            (setq default-directory "/tmp/"))
          (with-current-buffer (nth 1 tss-manager--project-list)
            (setq default-directory "/var/"))
          (should (tss-manager--containing-project-loaded? "/tmp/my.ts"))
          (should (tss-manager--containing-project-loaded? "/var/test.ts"))
          (should-not (tss-manager--containing-project-loaded? "/home/my/test.ts")))
      ;; clean up
      (loop for buf in tss-manager--project-list
            do (kill-buffer buf)))))


(ert-deftest )
