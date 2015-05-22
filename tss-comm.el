;;; TSS Communication: code that talks with TSS

(defun tss-comm/start (client)
  "Start a TSS service for CLIENT."
  (let ((tss-bin (executable-find "tss"))
        procnm cmdstr proc)
    (unless tss-bin
      (error "tss command not found"))
    (with-slots (name type buffer) client
      (pcase type
        (`file
         (setq procnm name
               cmdstr (format "%s %s" tss-bin (buffer-file-name buffer))))
        (`tsconfig
         (setq procnm (format "tss-%s" tss-project--name)
               cmdstr tss-bin))
        (_ (error "Client type [%s] is NOT supported." type))))
    proc))

;;;#NO-TEST
(defun* tss-comm--connect (&key type client)
  "Connect to TSS service for CLIENT, an file or project buffer.
TYPE is 'file or 'tsconfig."
  (let ((tss-bin (executable-find "tss"))
        procnm cmdstr)
    (unless tss-bin
      (error "tss command not found"))
    (with-current-buffer client
      ;; pre-start setup
      (pcase type
        (`file
         (setq procnm (format "tss-%s" (f-filename (buffer-file-name)))
               cmdstr (format "%s %s" tss-bin (buffer-file-name))))
        (`tsconfig
         (setq procnm (format "tss-%s" tss-project--name)
               cmdstr tss-bin))
        (_ (error "Client type [%s] is NOT supported." type)))
      ;; start process
      (let* ((process-connection-type nil)
             ;; note though the process buffer is set, a filter function
             ;; `tss--receive-server-response' is used for input handling,
             ;; see below.
             (proc (start-process-shell-command procnm client cmdstr))
             (waiti 0))
        (when proc
          (set-process-filter proc #'tss--receive-server-response)
          (set-process-sentinel proc #'tss--proc-sentinel)
          (set-process-query-on-exit-flag proc nil)
          (setq tss-client--server-response nil
                tss-client--incomplete-server-response ""
                tss-client--json-response-start-char ""
                tss-client--json-response-end-char "")
          ;; TODO 25 is magic, find a way to sanitize it.
          (while (and (< waiti 25)
                      (not tss-client--server-response))
            (accept-process-output proc 0.2 nil t)
            (sleep-for 0.2)
            (incf waiti))
          (tss--info "Finished start tss process.")
          (when (eq tss-client--server-response 'succeed)
            (tss--show-message "Loaded '%s'." (buffer-name)))
          proc)))))
