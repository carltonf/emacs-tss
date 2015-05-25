;;; TSS Communication: code that talks with TSS

;;;#NO-TEST
(defun tss-comm/start (client)
  "Start a TSS service for CLIENT."
  (let ((tss-bin (executable-find "tss"))
        procnm cmdstr proc
        (process-connection-type nil)
        (waiti 0))
    (unless tss-bin
      (error "tss command not found"))
    ;; prepare process
    (with-slots (name
                 type
                 buffer
                 ;; communication
                 response
                 incomplete-response
                 response-start-tag
                 response-end-tag) client
      (setq procnm (format "tss-%s") name)
      (pcase type
        (`file
         (setq cmdstr (format "%s %s" tss-bin (buffer-file-name buffer))))
        (`tsconfig
         (setq cmdstr tss-bin))
        (_ (error "Client type [%s] is NOT supported." type)))
      ;; start a process
      (setq proc (start-process-shell-command procnm nil cmdstr))
      ;; configure the proc
      (when proc
        (process-put proc 'client client)
        (setq response nil
              incomplete-response ""
              response-start-tag ""
              response-end-tag "")
        (set-process-query-on-exit-flag proc nil)

        (set-process-filter proc #'tss-comm/receive-response)
        (set-process-sentinel proc #'tss--proc-sentinel)

        ;; TODO 25 is magic, find a way to sanitize it.
        (while (and (< waiti 25)
                    (not tss-client--server-response))
          (accept-process-output proc 0.2 nil t)
          (sleep-for 0.2)
          (incf waiti))
        (tss--info "Finished start tss process.")
        (when (eq tss-client--server-response 'succeed)
          (tss--show-message "Loaded '%s'." (buffer-name)))
        proc))
    proc))

;;;#NO-TEST
(defun tss-comm/response-balanced? (str stag etag)
  "Check whether STR is balanced with STAG as start tag, ETAG as end tag."
  (cond
   ;; no tags means always balanced
   ((or (s-blank? stag "")
        (s-blank? etag ""))
    t)
   ;; normal case
   ((and (s-prefix? stag str)
         (s-suffix? etag str))
    ;; TODO the following use of `json-mode' feels bad...
    (with-temp-buffer
      (insert str)
      (json-mode)
      (goto-char (point-max))
      (ignore-errors (backward-list))
      (= (point) (point-min))))
   ;; other situations?
   (_ nil)))

;;;#NO-TEST
(defun tss-comm/receive-response (proc res)
  "Process filter for TSS.

Output from processes can only get processed when Emacs becomes
idle and it's possible to only receive partial result.

TSS for now has informal/various output format. TSServer on the
other unify outputs in standard JSON format."
  (let ((client (process-get proc 'client)))
    (with-slots (response
                 incomplete-response
                 response-start-tag
                 response-end-tag) client
      (loop with endre = (rx-to-string `(and bol "\"" (or "loaded" "updated" "added")
                                             (+ space)))
            for line in (split-string (or res "") "[\r\n]+")
            if (s-equals? line "null")
            return (progn (message "TSS: Got null response")
                          (setq response 'null))
            ;; normal JSON style response
            if (and (s-present? line)
                    (or
                     ;; in the middle of receiving response
                     (s-present? incomplete-response)
                     ;; start to get response 
                     (s-prefix? response-start-tag line)))
            return (progn (tss--debug "Got json response : %s" line)
                          (setq incomplete-response (concat incomplete-response line))
                          (when (tss-comm/response-balanced? incomplete-response
                                                             response-start-tag
                                                             response-end-tag)
                            (tss--trace "Finished getting json response")
                            (setq response (json-read-from-string incomplete-response)
                                  incomplete-response "")))
            ;; special output: a line of string with special format
            if (string-match endre line)
            return (progn (tss--debug "Got other response : %s" line)
                          (setq response 'succeed))
            ;; error for server
            if (string-match "\\`\"TSS +\\(.+\\)\"\\'" line)
            do (tss-comm/handle-err-response (match-string-no-properties 1 line))))))

;;; TODO know better about possible error conditions
;;;#+NO-TEST
(defun tss-comm/handle-err-response (res)
  (tss--trace "Handle error response : %s" res)
  (cond ((string= res "closing")
         nil)
        ((string-match "\\`command syntax error:" res)
         nil)
        (t
         (tss--debug "Got error response : %s" res)
         (tss--show-message "%s" res))))
