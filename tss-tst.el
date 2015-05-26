;;; TS Communication for "clausreinke/typescript-tools"

(defclass tss-tst/class (tss-comm/class)
  ((proc :type process
         :initform nil
         :documentation "Process of tss.")
   (incomplete-response :type string
                        :initform ""
                        :documentation "Incomplete/intermediate TSS response, raw JSON string.")
   (response-start-tag :type string
                       :initform ""
                       :documentation "Indicate the start of the response.")
   (response-end-tag :type string
                     :initform ""
                     :documentation "Indicate the end of the response."))
  :allow-nil-initform t
  :documentation
  "TS service class for \"clausreinke/typescript-tools\".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Implementing interface

(defmethod tss-tst/get-cmdstr ((this tss-tst/class))
  "Construct a command string to be used by
`start-process-shell-command'."
  (let ((tss-bin (executable-find "tss"))
        cmdstr)
    (unless tss-bin
      (error "tss command not found"))
    (with-slots (name type buffer) (oref this client)
      (pcase type
        (`file
         (setq cmdstr (format "%s %s" tss-bin (buffer-file-name buffer))))
        (`tsconfig
         (setq cmdstr tss-bin))
        (_ (error "Client type [%s] is NOT supported." type))))
    cmdstr))

;;;#NO-TEST
(defmethod tss-comm/start ((this tss-tst/class))
  "Start a TSS service for CLIENT."
  (let (client-name procnm cmdstr
        (process-connection-type nil)
        (waiti 0))
    ;; prepare process
    (with-slots (client proc response incomplete-response
                 response-start-tag response-end-tag) this
      (setq client-name (oref client name)
            procnm (format "tss-%s" client-name)
            cmdstr (tss-tst/get-cmdstr this))
      ;; start a process
      (setq proc (start-process-shell-command procnm nil cmdstr))
      ;; configure the proc
      (when proc
        ;; pass communication object with process property see
        ;; `tss-tst/receive-response-filter'.
        (process-put proc 'comm this)
        (setq response nil
              incomplete-response ""
              response-start-tag ""
              response-end-tag "")
        (set-process-query-on-exit-flag proc nil)

        (set-process-filter proc #'tss-tst/receive-response-filter)
        (set-process-sentinel proc #'tss-tst/proc-sentinel)

        ;; TODO 25 is magic, find a way to sanitize it.
        (while (and (< waiti 25) (not response))
          (accept-process-output proc 0.2 nil t)
          (incf waiti))
        (when (eq response 'succeed)
          (message "TSS: Loaded '%s'." client-name))
        proc))))

;;;#NO-TEST
(defun tss-tst/proc-sentinel (proc event)
  "TSS process sentinel"
  (let ((comm (process-get proc 'comm)))
    (tss-tst/sentinel comm proc event)))

;;;#NO-TEST
(defmethod tss-tst/sentinel ((this tss-tst/class) proc event)
  (message "TSS: %s had the event: %s" proc event)
  (let ((pstat (process-status proc)))
    (oset this status (pcase pstat
                        (`run :active)
                        (_ :inactive))))
  (tss-comm/sentinel this))

;;; TODO how to set up a user event system s.t. we can hook things up
;; update mode line
;; TODO: how to set all buffers within a project for now let's do it with a timer
;; (with-current-buffer (window-buffer)
;;   (tss--set-status-mode-line-str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Internal methods

;;;#NO-TEST
(defun tss-tst/receive-response-filter (proc res)
  "Glue function to bridge class methods with process filter functions.

Process property `comm' here is used as a reference for
communication object."
  (let ((comm (process-get proc 'comm)))
    (tss-tst/receive-response comm proc res)))

(defmethod tss-tst/receive-response ((this tss-tst/class) proc res)
  "Process filter for TSS.

Output from processes can only get processed when Emacs becomes
idle and it's possible to only receive partial result.

TSS for now has informal/various output format. TSServer on the
other unify outputs in standard JSON format."
  (with-slots (response
               incomplete-response
               response-start-tag
               response-end-tag) this
    (loop with endre = (rx-to-string `(and bol "\"" (or "loaded" "updated" "added")
                                           (+ space))) ;Note: quoted string within.
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
                   (and (s-present? response-start-tag)
                        (s-prefix? response-start-tag line))))
          return (progn (tss--debug "Got json response : %s" line)
                        (setq incomplete-response (concat incomplete-response line))
                        (when (tss-tst/response-balanced? incomplete-response
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
          do (tss-tst/handle-err-response (match-string-no-properties 1 line)))))

;;; TODO know better about possible error conditions
;;;#+NO-TEST
(defmethod tss-tst/handle-err-response ((this tss-tst/class) res)
  (tss--trace "Handle error response : %s" res)
  (cond ((string= res "closing")
         nil)
        ((string-match "\\`command syntax error:" res)
         nil)
        (t
         (tss--debug "Got error response : %s" res)
         (tss--show-message "%s" res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Static functions

(defun tss-tst/response-balanced? (str stag etag)
  "Check whether STR is balanced with STAG as start tag, ETAG as end tag."
  (cond
   ;; no tags means always balanced
   ((or (s-blank? stag)
        (s-blank? etag))
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
   (t nil)))

(provide 'tss-tst)
