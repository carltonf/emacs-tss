;;; TS Communication for "clausreinke/typescript-tools"

(defclass tss-tst/class (tss-comm/class)
  ((proc :type process
         :initform nil
         :documentation "Process of tss.")
   (incomplete-response :type string
                        :initform ""
                        :documentation "Incomplete/intermediate TSS response, raw JSON string.")
   ;; WARNING: TSS response is NOT JSON actually, it's more like JavaScript data
   ;; get inspected. For now, there are string, array and object. So here we
   ;; need to set the start&end char to know what responses we are receiving.
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

(defmethod tss-tst/get-start-cmdstr ((this tss-tst/class))
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
    (with-slots (client status proc response incomplete-response
                 response-start-tag response-end-tag) this
      (setq client-name (oref client name)
            procnm (format "tss-%s" client-name)
            cmdstr (tss-tst/get-start-cmdstr this))
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

        (tss-tst/accept-response this)

        (when (eq response 'succeed)
          (setq status :active)
          (message "TSS: Loaded '%s'." client-name))
        proc))))

;;;#NO-TEST
(defmethod tss-tst/accept-response ((this tss-tst/class) &optional timeout)
  "Accept response from ts-tools.

TIMEOUT is the seconds to wait before return, default to 10 sec.
Return response in the end, which is possibly nil.

The algorithm is busy polling. To ensure the completeness of
response, TIMEOUT is usually quite large, so it is sliced into
multiple short intervals such that at each interval we can check
whether a complete response has been received to return
immediately."
  ;; 0.25 is the trial-and-error magic
  (let* ((slice 0.25)
         (int-count (floor (/ (or timeout 10) slice))))
    (with-slots (proc response) this
      (while (and (> int-count 0)
                  (null response))
          (accept-process-output proc 0.2 nil t)
          (decf int-count))
      response)))

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

(defmethod tss-tst/receive-response ((this tss-tst/class) proc rawres)
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
          for line in (split-string (or rawres "") "[\r\n]+")
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
          do (tss-tst/handle-err-response this line rawres))))

;;; TODO know better about possible error conditions
;;;#+NO-TEST
(defmethod tss-tst/handle-err-response ((this tss-tst/class) line res)
  (warn "TST Handled Errors: [%s] in [%s]" line res)
  ;; (cond ((string= res "closing")
  ;;        nil)
  ;;       ((string-match "\\`command syntax error:" res)
  ;;        nil)
  ;;       (t
  ;;        (tss--debug "Got error response : %s" res)
  ;;        (tss--show-message "%s" res)))
  )

;;;#NO-TEST
(defmethod tss-comm/destroy ((this tss-tst/class))
  (delete-process (oref this proc))
  (call-next-method))

(defconst tss-tst/supported-cmds
  '(;; <cmd> <line> <pos> <file>
    "quickInfo" "definition" "references" "completions" "completions-brief"
    ;; <cmd> <file>/<item>
    "navigationBarItems" "navigateToItems"
    ;; <cmd> (nochech)? <linecount> <file> [linecount lines of source text]
    "update"
    ;; <cmd>
    "reload" "files" "showErrors" "quit")
  "A list of supported commands of typescript-tools, see
https://github.com/clausreinke/typescript-tools for the complete
list and docs.")


;;;#NO-TEST
(defmethod tss-comm/command-inspect ((this tss-tst/class) comm-cmds)
  "Interactive command to send arbitrary command to
typescript-tools.

COMM-CMDS is a list, whose car should be one of
`tss-tst/supported-cmds'."
  (with-slots (response-start-tag
               response-end-tag
               response
               client) this
    (let* ((cbuf (oref client buffer))
           (cmd (car comm-cmds))
           cmdstr
           (posarg (tss-tst/get-posarg cbuf))
           (file (buffer-file-name cbuf)))
      (pcase cmd
        ((or "quickInfo" "definition" "references" "completions" "completions-brief")
         (setq cmdstr (format "%s %s %s" cmd posarg file)))
        ("navigationBarItems"
         (setq cmdstr (format "%s %s" cmd file)))
        ("navigateToItems"
         (let ((item (symbol-at-point)))
           (setq cmdstr (format "%s %s" cmd item))))
        ((or "files" "showErrors")
         (setq cmdstr (format "%s" cmd)))
        ("reload"
         (message "reload currently doesn't respond any meaningful response back."))
        (_
         (error "%s NOT supported yet ;P" cmd)))
      ;; different commands have different delimiters
      ;; TODO feels cumbersome. An adaptive receiver?
      (pcase cmd
        ((or "quickInfo" "definition" "completions" "completions-brief")
         (setq response-start-tag "{"
               response-end-tag "}"))
        ((or "references" "navigationBarItems" "navigateToItems" "files" "showErrors")
         (setq response-start-tag "["
               response-end-tag "]"))
        (_
         (setq response-start-tag ""
               response-end-tag "")))

      (tss-tst/send-accept this cmdstr)
      response)))

;;;#NO-TEST
(defmethod tss-tst/send-accept ((this tss-tst/class) msg)
  "Helper method. Issuing most commands require clearing response and this 'send-accept' steps.
Return response.

NOTE: don't use this method on command that don't return output."
  (tss-tst/send-msg this msg)
  (tss-tst/accept-response this))

;;;#NO-TEST
(defmethod tss-tst/send-msg ((this tss-tst/class) msg)
  "Send MSG to typescript tools instance and get the response.

Usually MSG is command string, but it can also be updated source
and etc."
  (with-slots (proc response incomplete-response) this
    (setq response nil
          incomplete-response "")

    ;; TODO error handling
    (process-send-string proc (concat msg "\n"))))

;;;#NO-TEST
(defun tss-tst/cmd-inspect-display (cmd)
  "ELisp interactive command wrapper around
`tss-comm/command-inspect', using `tss-client' local variable."
  (interactive (list
                (progn
                  ;; TODO we need to check the status of `tss-client'
                  (unless tss-client
                    (error "TSS: No active tss-client found."))
                  (completing-read "TS Command: "
                                   tss-tst/supported-cmds
                                   nil t))))
  (pp-display-expression (tss-client/comm-inspect tss--client (list cmd))
                         "*TST CMD Inspect*"))

;;;: API implementations
;;;#NO-TEST
(defmethod tss-comm/alive? ((this tss-tst/class))
  (with-slots (client proc status) this
    (and (tss-client/class-child-p client) ;DO NOT CHECK client aliveness!
         (eq status :active)
         ;; process checking (just for safety)
         proc
         (processp proc)
         (eq (process-status proc) 'run))))

;;;#NO-TEST
(defmethod tss-comm/update-source ((this tss-tst/class)
                                   source linecount path)
  ;; Work around a bug about ending blank line. In Emacs, if END is at the
  ;; beginning of a line, `count-lines' won't count the line END is at. However
  ;; ts-tools counts this blank line. A blank string sent would result an error
  ;; (TODO I consider this a bug in ts-tools that should get fixed). So to avoid
  ;; such an error, `incf' linecount.
  (when (s-matches-p "[\n\C-m]\\'" source)
    (incf linecount))

  (let ((cmdstr (format "update %d %s" linecount path)))
    ;; update doesn't output, don't accept
    (tss-tst/send-msg this cmdstr)
    (with-slots (response-start-tag
                 response-end-tag) this
      (setq response-start-tag ""
            response-end-tag ""))

    (tss-tst/send-accept this source)
    (unless (eq (oref this response) 'succeed)
      (warn "TSS: Fail to update source for '%s'." path))))

;;;#NO-TEST
(defmethod tss-comm/get-completions ((this tss-tst/class)
                                     line column fpath)
  ;; in 'tst', "completions" command return a lot extra details
  (let ((cmdstr (format "completions-brief %d %d %s"
                        line (1+ column) fpath)))
    (with-slots (response-start-tag
                 response-end-tag
                 response) this
      (setq response-start-tag "{"
            response-end-tag "}")
      (tss-tst/send-accept this cmdstr)
      ;; for completions, `null' means no completions and thus should be nil.
      (when (eq response 'null)
        (setq response nil))
      ;; make sure to return response
      response)))

(defmethod tss-comm/get-doc ((this tss-tst/class)
                             line column fpath)
  (let ((cmdstr (format "quickInfo %d %d %s"
                        line (1+ column) fpath)))
    (with-slots (response-start-tag
                 response-end-tag) this
      (setq response-start-tag "{"
            response-end-tag "}")
      (tss-tst/send-accept this cmdstr))))

(defmethod tss-comm/get-errors ((this tss-tst/class))
  (let ((cmdstr (format "showErrors")))
    (with-slots (response-start-tag
                 response-end-tag) this
      (setq response-start-tag "["
            response-end-tag "]")
      (tss-tst/send-accept this cmdstr))))

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
    ;; TODO the following use of `json-mode' feels bad... possible alts:
    ;; jsonlint, `flymake-json' and etc, maybe even the built in
    ;; `json-readtable'?
    (with-temp-buffer
      (insert str)
      (json-mode)
      (goto-char (point-max))
      (ignore-errors (backward-list))
      (= (point) (point-min))))
   ;; other situations?
   (t nil)))

(defun tss-tst/get-posarg (buffer)
  "Get position argument in the format '<line-num> <col-num>'.

NOTE that Emacs count column from 0, but typescript tools expect
column number from 1."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (format "%d %d"
              (line-number-at-pos)
              (1+ (- (point) (line-beginning-position)))))))

(provide 'tss-tst)
