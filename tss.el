;;; tss.el --- provide a interface for auto-complete.el/flymake.el on typescript-mode.

;; Copyright (C) 2013-2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: typescript, completion
;; URL: https://github.com/aki2o/emacs-tss
;; Version: 0.6.0
;; Package-Requires: ((auto-complete "1.4.0") (json-mode "1.1.0") (log4e "0.2.0") (yaxception "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This extension is a interface for typescript-tools.
;; This extension provides the following on typescript-mode.
;;  - Auto completion by using auto-complete.el
;;  - Check syntax by using flymake.el
;;
;; For more infomation,
;; see <https://github.com/aki2o/emacs-tss/blob/master/README.md>

;;; Dependency:
;;
;; - typescript.el ( see <http://www.typescriptlang.org/> )
;; - typescript-tools ( see <https://github.com/clausreinke/typescript-tools> )
;; - auto-complete.el ( see <https://github.com/auto-complete/auto-complete> )
;; - json-mode.el ( see <https://github.com/joshwnj/json-mode> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;;
;; (require 'tss)

;;; Configuration:
;;
;; ;; Key Binding
;; (setq tss-popup-help-key "C-:")
;; (setq tss-jump-to-definition-key "C->")
;;
;; Make config suit for you. About the config item, see Customization
;; or eval the following sexp.
;;
;; (customize-group "tss")
;;
;; (tss-config-default)

;;; Customization:
;;
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "tss-[^\-]" :docstring t)
;; `tss-popup-help-key'
;; Keystroke for `tss-popup-help' at point.
;; `tss-jump-to-definition-key'
;; Keystroke for `tss-jump-to-definition' at point.
;; `tss-implement-definition-key'
;; Keystroke for `tss-implement-definition' at point.
;; `tss-enable-modes'
;; Major modes TSS is enabled on.
;; `tss-ac-trigger-command-keys'
;; Keystrokes for doing `ac-start' with self insert.
;; `tss-inactive-code-faces'
;; Faces not considered a code part.
;; `tss-ac-summary-truncate-length'
;; Length for truncation of candidate summary of auto-complete.el.
;;
;;  *** END auto-documentation

;;; API:
;;
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "tss-[^\-]" :docstring t)
;; `tss-popup-help'
;; Popup help about anything at point.
;; `tss-jump-to-definition'
;; Jump to method definition at point.
;; `tss-implement-definition'
;; Implement inherited definitions of current component.
;; `tss-run-flymake'
;; Run check by flymake for current buffer.
;; `tss-reload-current-project'
;; Reload project data for current buffer.
;; `tss-restart-current-buffer'
;; Restart TSS for current buffer.
;; `tss-stop-current-buffer'
;; Stop TSS for current buffer.
;; `tss-setup-current-buffer'
;; Do setup for using TSS in current buffer.
;;
;;  *** END auto-documentation
;;
;; [Note] Functions and variables other than listed above, Those
;; specifications may be changed without notice.

;;; Tested On:
;;
;; - Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
;; - typescript-tools ... Version For Typescript v0.9
;; - auto-complete.el ... Version 1.4.0
;; - json-mode.el ... Version 1.1.0
;; - yaxception.el ... Version 0.1
;; - log4e.el ... Version 0.2.0


;; Enjoy!!!

;;; Code:

(eval-when-compile (require 'cl))
(require 'auto-complete)
(require 'json-mode)
(require 'json)
(require 'ring)
(require 'etags)
(require 'flymake)
(require 'eldoc)
(require 'pos-tip nil t)
(require 'anything nil t)
(require 'helm nil t)
(require 'log4e)
(require 'yaxception)

(defgroup tss nil
  "Auto completion / Flymake for TypeScript."
  :group 'completion
  :prefix "tss-")

(defcustom tss-popup-help-key nil
  "Keystroke for `tss-popup-help' at point."
  :type 'string
  :group 'tss)

(defcustom tss-jump-to-definition-key nil
  "Keystroke for `tss-jump-to-definition' at point."
  :type 'string
  :group 'tss)

(defcustom tss-implement-definition-key nil
  "Keystroke for `tss-implement-definition' at point."
  :type 'string
  :group 'tss)

(defcustom tss-enable-modes '(typescript-mode)
  "Major modes TSS is enabled on."
  :type '(repeat symbol)
  :group 'tss)

(defcustom tss-ac-trigger-command-keys '("SPC" "." ":" "<")
  "Keystrokes for doing `ac-start' with self insert."
  :type '(repeat string)
  :group 'tss)

(defcustom tss-inactive-code-faces '(font-lock-comment-face
                                     font-lock-string-face)
  "Faces not considered a code part."
  :type '(repeat symbol)
  :group 'tss)

(defcustom tss-ac-summary-truncate-length 22
  "Length for truncation of candidate summary of auto-complete.el."
  :type 'integer
  :group 'tss)

(defface tss-eldoc-function-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for the part of function in eldoc."
  :group 'tss)

(defface tss-eldoc-type-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the part of type in eldoc."
  :group 'tss)


(log4e:deflogger "tss" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                 (error . "error")
                                                 (warn  . "warn")
                                                 (info  . "info")
                                                 (debug . "debug")
                                                 (trace . "trace")))
(tss--log-set-level 'trace)

(yaxception:deferror 'tss-command-not-found nil "Not found 'tss' command")


;;;;;;;;;;;;;
;; Utility

(defvar tss--builtin-keywords
  '("any" "break" "case" "class" "constructor" "continue" "declare" "default"
    "do" "enum" "export" "extends" "false" "function" "get" "implements"
    "import" "in" "interface" "module" "new" "null" "number" "private" "public"
    "return" "set" "static" "super" "switch" "this" "true" "undefined" "until"
    "var" "void" "while" "with"))

(defvar tss--builtin-special-comments
  '(("reference" . ("path" "no-default-lib"))
    ("summary"   . nil)
    ("param"     . ("name" "type"))
    ("value"     . ("type"))))

(defvar-local tss--last-send-string-failed-p nil)
(defvar-local tss--current-active-p t)

(defmacro tss--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defun* tss--show-message (msg &rest args)
  (apply 'message (concat "[TSS] " msg) args)
  nil)

(defsubst tss--active-p ()
  (and tss--current-active-p
       (memq major-mode tss-enable-modes)
       t))

(defsubst tss--active-code-point-p (&optional pt)
  (let ((fc (get-text-property (or pt (point)) 'face)))
    (not (memq fc tss-inactive-code-faces))))

(defsubst tss--search-backward-in-code (str &optional limitpt)
  (loop while (search-backward str limitpt t)
        if (tss--active-code-point-p) return (point)))

(defsubst tss--search-forward-in-code (str &optional limitpt)
  (loop while (search-forward str limitpt t)
        if (tss--active-code-point-p) return (point)))

(defsubst tss--get-position-argument ()
  (save-excursion
    (widen)
    (format "%d %d"
            (line-number-at-pos)
 			(+ (- (point) (line-beginning-position)) 1))))

(defconst tss--service-supported-cmds
  '(;; <cmd> <line> <pos> <file>
    "quickInfo" "definition" "references" "completions" "completions-brief"
    ;; <cmd> <file>/<item>
    "navigationBarItems" "navigateToItems"
    ;; <cmd> (nochech)? <linecount> <file> [linecount lines of source text]
    "update"
    ;; <cmd>
    "reload" "files" "showErrors" "quit")
  "A list of supported commands of ts service, see
https://github.com/clausreinke/typescript-tools
for the complete list and docs.")

;;;###autoload
(defun tss-send-command-display (cmd &optional cmdargs)
  "Interactive command to send arbitrary command to typescript
service and display raw feedback. Mainly used for
debugging/development.

If CMDARGS is set, it should be a full command argument list,
which can be line number, current column (note that Emacs count
column from 0, ts service starts from 1) or full file path and
etc. Otherwise, generate these args according to `current-buffer'
and `point'."
  (interactive (list
                (progn
                  (unless (tss--active-p)
                    (error "Only used in active TSS buffer."))
                  (completing-read "TS Command: "
                                   tss--service-supported-cmds
                                   nil t))))
  (let ((resp-display-bufnm "*TSS Response*")
        cmdstr resp
        (response-start-char "{")
        (response-end-char "}"))
    (pcase cmd
      ((or "quickInfo" "definition" "references" "completions" "completions-brief")
       (let ((posarg (tss--get-position-argument))
             (file (buffer-file-name)))
         (setq cmdstr (format "%s %s %s" cmd posarg file))))
      ("navigationBarItems"
       (let ((file (buffer-file-name)))
         (setq cmdstr (format "%s %s" cmd file))))
      ("navigateToItems"
       (let ((item (symbol-at-point)))
         (setq cmdstr (format "%s %s" cmd item))))
      ((or "files" "showErrors")
       (setq cmdstr (format "%s" cmd)))
      ("reload"
       (message "reload currently doesn't respond any meaningful response back."))
      (_
       (error "%s NOT supported yet ;P" cmd)))
    ;; here we fiddle with the start/end char of response, this is needed by
    ;; `tss--get-server-response' and `tss--receive-server-response'. Response
    ;; delimiters can also be found on ts tools page.
    ;; TODO is this cumbersome start/end char really needed? An adaptive
    ;; receiver?
    (pcase cmd
      ((or "quickInfo" "definition" "completions" "completions-brief")
       (setq response-start-char "{"
             response-end-char "}"))
      ((or "references" "navigationBarItems" "navigateToItems" "files" "showErrors")
       (setq response-start-char "["
             response-end-char "]")))
    ;; send and display
    (setq resp (tss--get-server-response
                cmdstr :waitsec 2
                :response-start-char response-start-char
                :response-end-char response-end-char))
    (pp-display-expression resp resp-display-bufnm)))

(defsubst tss--send-string (proc sendstr)
  (yaxception:$
    (yaxception:try
      (when proc
        (process-send-string proc (concat sendstr "\n"))
        t))
    (yaxception:catch 'error e
      (tss--error "failed send string : %s" (yaxception:get-text e))
      (error "tss--send-string is gonna kill TSS.")
      ;; (tss--delete-process t)
      (if (not tss--last-send-string-failed-p)
          (setq tss--last-send-string-failed-p t)
        (setq tss--current-active-p nil)
        (tss--popup-tip
         (concat
          "Stopped TSS by errored on TypeScript Services Server.\n"
          "Maybe it be caused by the incomplete source code in buffer.\n"
          "At later, execute `tss-restart-current-buffer' for restart TSS."))
        (sleep-for 2))
      nil)))

(defsubst tss--stringify-response-element (e)
  (or (and e (downcase (format "%s" e)))
      "unknown"))

(defsubst tss--function-kind-p (kind)
  (member kind '("function" "method" "constructor" "local function")))

(defun tss--popup-tip (text)
  (if (and (functionp 'ac-quick-help-use-pos-tip-p)
           (ac-quick-help-use-pos-tip-p))
      (pos-tip-show text 'popup-tip-face nil nil 300 popup-tip-max-width)
    (popup-tip text)))

(defun tss--get-current-statement-startpt ()
  (save-excursion
    (or (tss--awhen (tss--search-backward-in-code ";")
          (+ it 1))
        (point-min))))

(defun tss--get-current-param-startpt (&optional limitpt)
  (save-excursion
    (loop with limitpt = (or limitpt (point-min))
          for pt = (loop with depth = 1
                         for bspt = (or (save-excursion
                                          (tss--search-backward-in-code "(" limitpt))
                                        (point-min))
                         for bept = (or (save-excursion (tss--search-backward-in-code ")" limitpt))
                                        (point-min))
                         while (> (point) limitpt)
                         do (cond ((> bspt bept) (decf depth) (goto-char bspt))
                                  (t             (incf depth) (goto-char bept)))
                         if (= depth 0) return (point)
                         finally return limitpt)
          while (> pt limitpt)
          if (string-match "\\`[a-zA-Z0-9_$]\\'" (format "%c" (char-before pt)))
          return pt)))

(defun tss--buffer-substring-in-code (startpt endpt)
  (save-excursion
    (loop initially (goto-char startpt)
          with ret = ""
          while (< (point) endpt)
          for nextpt = (or (next-single-property-change (point) 'face nil endpt)
                           endpt)
          if (tss--active-code-point-p)
          do (setq ret (concat ret (buffer-substring-no-properties (point) nextpt)))
          do (goto-char nextpt)
          finally return ret)))

(defun tss--remove-brace-part (str)
  (loop for nstr = (replace-regexp-in-string "([^(]*)" "" str)
        while (not (string= str nstr))
        do (setq str nstr)
        finally return nstr))

(defun tss--make-temp-buffer (code)
  (with-current-buffer (get-buffer-create " *tss temp*")
    (delete-region (point-min) (point-max))
    (insert code)
    (current-buffer)))

(defun tss--build-method-definition (name type &optional not-propertize)
  (concat
   (if not-propertize
       name
     (propertize name 'face 'tss-eldoc-function-name-face))
   (with-temp-buffer
     (insert type)
     (when (string= (format "%c" (char-before)) ")")
       (backward-list 1)
       (skip-syntax-backward " ")
       (delete-region (point) (point-max)))
     (goto-char (point-min))
     (forward-char 1)
     (while (search-forward ":" nil t)
       (skip-syntax-forward " ")
       (when (not not-propertize)
         (put-text-property (point)
                            (cond ((string= (format "%c" (char-after)) "(")
                                   (forward-list)
                                   (re-search-forward " +=> +[^ ,)]+" nil t)
                                   (point))
                                  ((re-search-forward "[ ,)]" nil t)
                                   (- (point) 1))
                                  (t
                                   (goto-char (point-max))
                                   (point)))
                            'face
                            'tss-eldoc-type-face)))
     (buffer-string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For typescript-tools

;; local variable for current TS prj setup {{
(defvar-local tss--proc nil
  "Current TSS process for this buffer, in case of TS project all
project buffers share a single TS process.")
(defvar-local tss--project nil
  "Root path of the TS project current buffer belongs to. For
a single TS file, it's the path of this file.")
(defvar-local tss--project-buffer nil
  "Buffer for TS project, in case no project is configured the
file buffer is returned.")
;; }}

;; local variables for parsing/storing tss service {{
(defvar-local tss--server-response nil
  "Main variable for storing TSS response. Though this is
buffer-local var, for project files only the ones set in project
buffer are meaningful.")
(defvar-local tss--incomplete-server-response ""
  "Intermediate response content. see `tss--server-response' for
more info.")
;; TODO the following two are bad naming, which I believe is a problem with TSS.
(defvar-local tss--json-response-start-char ""
  "Start character of response string, paired with `tss--json-response-end-char'.

WARNING: TSS response is NOT JSON actually, it's more like
JavaScript data get inspected. For now, there are string, array
and object. So here we need to set the start&end char to know
what responses we are receiving.")
(defvar-local tss--json-response-end-char ""
  "End character of response string.

See `tss--json-response-start-char' for more info.")
;; }}

;;; TODO depreciated?
;; (defun tss--get-process-create (&optional initializep)
;;   "Get a TSS process for current buffer, create one if no process
;; can be found.

;; If `tss--proc' is not set for current buffer,
;; `tss--project-runtime-table' will be looked up to see whether the
;; current buffer's file is within some existing TSS project and get
;; that TSS process. As a last resort a new TSS process will start."
;;   (let ((fpath (expand-file-name (buffer-file-name)))
;;         proc)
;;     (cond
;;      ((and (processp tss--proc)
;;            (eq (process-status (process-name tss--proc)) 'run))
;;       tss--proc)
;;      ((-some? (lambda (record-path)
;;                 (when (string-prefix-p record-path fpath)
;;                   (setq proc (buffer-local-value
;;                               'tss--proc
;;                               (gethash record-path tss--project-runtime-table)))
;;                   ;; test process status?
;;                   t))
;;               (hash-table-keys tss--project-runtime-table))
;;       proc)
;;      (t (tss--start-process initializep)))))

(defun tss--exist-process ()
  "Check whether `tss--proc' is set and *alive*."
  (and (processp tss--proc)
       (eq (process-status tss--proc) 'run)
       t))

(defvar tss--project-runtime-table (make-hash-table :test #'equal)
  "A table mapping source path or project root to TS project runtime buffer.
Here buffer is used as an object, whose local variables hold all
runtime configurations for the project. When a TS buffer is first
visited, `tss--setup-project-for-current-buffer' will lookup this
table to decide whether to start a new TSS.

NOTE the key should be canonized path.")

(defun tss--delete-process (&optional killp waitp prjpath)
  "Delete process `tss--proc'. If PRJPATH is non-nil, remove it
from `tss--project-runtime-table'."
  (yaxception:$
    (yaxception:try
      ;; clean process in `tss--proc'
      (when (tss--exist-process)
        (cond (killp
               (kill-process tss--proc)
               (when waitp (sleep-for 1)))
              (t
               (process-send-string proc "quit\n")
               (delete-process tss--proc)))
        (when prjpath
          (remhash prjpath tss--project-runtime-table))
        (setq tss--proc nil))
      t)
    (yaxception:catch 'error e
      (tss--error "failed delete process : %s" (yaxception:get-text e)))))

(defun tss-clean-all-service-processes ()
  "Helper command to delete all service processes."
  (interactive)
  (loop for proc in (process-list)
      when (s-starts-with? "typescript-service"
                           (process-name proc))
      do (delete-process proc))
  ;; TODO extra checking?
  (clrhash tss--project-runtime-table))

;; TODO support TS buffer without files
(defun* tss--start-process (&key projectp initializep)
  "Start a new process for current buffer or project, cleaning old processes beforehand.

If PROJECTP is non-nil, look `tss--project' up in
`tss--root-sources-project-configs-table'. If a project
configuration exists, instead of loading this file in TSS, use
sources configured to start a new TSS process.

NOTE: INITIALIZEP only has message difference."
  (when (not (executable-find "tss"))
    (yaxception:throw 'tss-command-not-found))
  (tss--trace "Start tss process for %s" (buffer-name))
  (with-current-buffer tss--project-buffer
    (let* ( ;; single file is considered as one-file virtual project
           (prjpath (if projectp
                        tss--project
                      (buffer-file-name)))
           (srcpaths (or (loop for path in (gethash prjpath tss--root-sources-project-configs-table)
                               ;; make sure the source paths are absolute
                               collect (expand-file-name path prjpath))
                         (list prjpath)))
           (procnm (format "typescript-service-%s" (file-name-nondirectory prjpath)))
           (cmdstr (format "tss %s"
                           (s-join " " (mapcar
                                        (lambda (path)
                                          (shell-quote-argument path))
                                        srcpaths))))
           (process-connection-type nil)
           (proc (when (file-exists-p prjpath)
                   (tss--delete-process t t (when projectp prjpath))
                   (tss--trace "Do %s" cmdstr)
                   (cond (initializep (tss--show-message "Load '%s' ..." (buffer-name)))
                         (t           (tss--show-message "Reload '%s' ..." (buffer-name))))
                   ;; note though the process buffer is set, a filter function
                   ;; `tss--receive-server-response' is used for input handling,
                   ;; see below.
                   (start-process-shell-command procnm tss--project-buffer cmdstr)))
           (waiti 0))
      (when proc
        (set-process-filter proc #'tss--receive-server-response)
        (set-process-sentinel proc #'tss--proc-sentinel)
        (set-process-query-on-exit-flag proc nil)
        (setq tss--server-response nil)
        (setq tss--incomplete-server-response "")
        (setq tss--json-response-start-char "")
        (setq tss--json-response-end-char "")
        (while (and (< waiti 50)
                    (not tss--server-response))
          (accept-process-output proc 0.2 nil t)
          (sleep-for 0.2)
          (incf waiti))
        (tss--info "Finished start tss process.")
        (when projectp
          ;; record this process for project
          (puthash prjpath tss--project tss--project-runtime-table))
        (when (eq tss--server-response 'succeed)
          (cond (initializep (tss--show-message "Loaded '%s'." (buffer-name)))
                (t           (tss--show-message "Reloaded '%s'." (buffer-name)))))
        ;; return proc
        proc))))

(defun tss--balance-json-brace-p (str startbrace endbrace)
  (if (or (string= startbrace "")
          (string= endbrace ""))
      t
    (and (string= (substring str 0 1) startbrace)
         (string= (substring str -1) endbrace)
         ;; (let* ((str (replace-regexp-in-string "\".*?[^\\\\]\"" "" str)) ; delete part of some text
         ;;        (str (replace-regexp-in-string "\"\"" "" str))           ; delete part of empty text
         ;;        (str (replace-regexp-in-string "\"[^\"]*\\'" "" str))    ; delete part of imcomplete text
         ;;        (delstartstr (replace-regexp-in-string (regexp-quote startbrace) "" str))
         ;;        (delendstr (replace-regexp-in-string (regexp-quote endbrace) "" str))
         ;;        (len (length str)))
         ;;   (= (- len (length delstartstr))
         ;;      (- len (length delendstr)))))))
         ;; Use json-mode as substitute for self parsing
         (with-temp-buffer
           (insert str)
           (json-mode)
           (goto-char (point-max))
           (ignore-errors (backward-list))
           (= (point) (point-min))))))

(defun tss--proc-sentinel (proc event)
  "TSS process sentinel"
  (message "TSS: %s had the event `%s'" proc event)
  ;; update mode line
  ;; TODO: how to set all buffers within a project
  ;; for now let's do it with a timer
  (with-current-buffer (window-buffer)
      (tss--set-status-mode-line-str)))

(defun tss--receive-server-response (proc res)
  (tss--trace "Received server response.\n%s" res)
  (with-current-buffer (process-buffer proc)
    (loop with endre = (rx-to-string `(and bol "\"" (or "loaded" "updated" "added")
                                           (+ space)))
          for line in (split-string (or res "") "[\r\n]+")
          if (string= line "null")
          return (progn (tss--debug "Got null response")
                        (setq tss--server-response 'null))
          if (and (not (string= line ""))
                  (or (not (string= tss--incomplete-server-response ""))
                      (string= (substring line 0 1) tss--json-response-start-char)))
          return (progn (tss--debug "Got json response : %s" line)
                        (setq tss--incomplete-server-response (concat tss--incomplete-server-response line))
                        (when (tss--balance-json-brace-p tss--incomplete-server-response
                                                         tss--json-response-start-char
                                                         tss--json-response-end-char)
                          (tss--trace "Finished getting json response")
                          (setq tss--server-response (json-read-from-string tss--incomplete-server-response))
                          (setq tss--incomplete-server-response "")))
          if (string-match endre line)
          return (progn (tss--debug "Got other response : %s" line)
                        (setq tss--server-response 'succeed))
          if (string-match "\\`\"TSS +\\(.+\\)\"\\'" line)
          do (tss--handle-err-response (match-string-no-properties 1 line)))))

(defun tss--handle-err-response (res)
  (tss--trace "Handle error response : %s" res)
  (cond ((string= res "closing")
         nil)
        ((string-match "\\`command syntax error:" res)
         nil)
        (t
         (tss--debug "Got error response : %s" res)
         (tss--show-message "%s" res))))

(defun* tss--get-server-response (cmdstr &key waitsec response-start-char response-end-char)
  (when (tss--active-p)
    (tss--debug "Start get server response. cmdstr[%s] waitsec[%s]" cmdstr waitsec)
    (let ((proc tss--proc)
          (waiti 0)
          (maxwaiti (* (or waitsec 3) 5)))
      (with-current-buffer (process-buffer proc)
        (setq tss--server-response nil)
        (setq tss--incomplete-server-response "")
        (setq tss--json-response-start-char (or response-start-char "{"))
        (setq tss--json-response-end-char (or response-end-char "}"))
        (when (tss--send-string proc cmdstr)
          (tss--trace "Start wait response from server.")
          (while (and (< waiti maxwaiti)
                      (not tss--server-response))
            (accept-process-output proc 0.2 nil t)
            (incf waiti))
          (cond ((not (< waiti maxwaiti))
                 (tss--warn "Timeout get response of %s" cmdstr)
                 nil)
                (t
                 (tss--trace "Got response from server.")
                 tss--server-response)))))))

(defun* tss--sync-server (&key waitsec buff path
                               source linecount)
  "Sync current source with tss service. Default to 15 waiti,
current buffer, current buffer's file path, current buffer's
content, current buffer's line count.

Use SOURCE and LINECOUNT to pass custom content and linecount.
These are useful when you are doing completing/templating when
the proposed changes are not even in buffer yet but you still
want to get some info about these supposed changes (like
definition/quickInfo and etc.). *NOTE*: I think these are needed
because the ts service only support stateless queries, but
nevertheless these might prove to be useful for other potential
source manipulation."
  (when (tss--active-p)
    (save-restriction
      (widen)
      (let ((proc tss--proc)
            (content (or source
                         (with-current-buffer (or buff (current-buffer))
                           (buffer-string))))
            (waiti 0)
            (maxwaiti (* (or waitsec 3) 5))
            (cmdstr (format "update %d %s"
                            (or linecount
                                (with-current-buffer (or buff (current-buffer))
                                  (count-lines (point-min) (point-max))))
                            (expand-file-name (or path (buffer-file-name))))))
        (tss--debug "Start sync server : %s" cmdstr)
        (with-current-buffer (process-buffer proc)
          (when (tss--send-string proc cmdstr)
            (setq tss--server-response nil)
            (setq tss--incomplete-server-response "")
            (setq tss--json-response-start-char "")
            (setq tss--json-response-end-char "")
            (tss--send-string proc content)
            (tss--trace "Start wait sync server.")
            (while (and (< waiti maxwaiti)
                        (not tss--server-response))
              (accept-process-output proc 0.2 nil t)
              (incf waiti))
            (cond ((not (< waiti maxwaiti))
                   (tss--warn "Timeout sync server.")
                   nil)
                  (t
                   (tss--trace "Finished sync server.")
                   (eq tss--server-response 'succeed)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For auto-complete.el

(defvar ac-source-tss-member
  '((candidates . tss--get-ac-member-candidates)
    (prefix . (tss--get-active-code-prefix "\\.\\([a-zA-Z0-9_]*\\)"))
    (requires . 0)
    (cache)))

(defvar ac-source-tss-type
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . (tss--get-active-code-prefix ": ?\\([a-zA-Z0-9_]*\\)"))
    (requires . 0)
    (cache)))

(defvar ac-source-tss-new
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . (tss--get-active-code-prefix "\\<new +\\([a-zA-Z0-9_]*\\)"))
    (requires . 0)
    (cache)))

(defvar ac-source-tss-extends
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . (tss--get-active-code-prefix " +extends +\\([a-zA-Z0-9_]*\\)"))
    (requires . 0)
    (cache)))

(defvar ac-source-tss-implements
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . (tss--get-active-code-prefix " +implements +\\([a-zA-Z0-9_]*\\)"))
    (requires . 0)
    (cache)))

(defvar ac-source-tss-tag
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . (tss--get-active-code-prefix "[^/] *<\\([a-zA-Z0-9_]*\\)"))
    (requires . 0)
    (cache)))

(defvar ac-source-tss-anything
  '((candidates . tss--get-ac-non-member-candidates)
    (prefix . (tss--get-active-code-prefix "\\(?:^\\|[^a-zA-Z0-9_.]\\) *\\([a-zA-Z0-9_]+\\)"))
    (requires . 1)
    (cache)))

(defvar ac-source-tss-keyword
  '((candidates . tss--get-ac-keyword-candidates)
    (prefix . (tss--get-active-code-prefix "\\(?:^\\|[^a-zA-Z0-9_.]\\) *\\([a-zA-Z0-9_]+\\)"))
    (requires . 1)
    (cache)))

(defvar ac-source-tss-special-comment
  '((candidates . tss--get-ac-special-comment-candidates)
    (prefix . "^[\t ]*///[^\n]*</?\\([a-zA-Z0-9:-]*\\)")
    (requires . 0)))

(defvar ac-source-tss-special-comment-attr
  '((candidates . tss--get-ac-special-comment-attribute-candidates)
    (prefix . "^[\t ]*///[^\n]*\\(?:<[a-zA-Z0-9:-]+\\|[^=]\"\\|[^=]'\\) +\\([a-zA-Z0-9-]*\\)")
    (requires . 0)
    (action . (lambda ()
                (insert "=\"")
                (when (and (not (= (point) (point-max)))
                           (string= (format "%c" (char-after)) "\""))
                  (delete-char 1))
                (insert "\"")
                (backward-char)
                (auto-complete)))))

(defvar ac-source-tss-referenc-path
  '((candidates . tss--get-ac-reference-path-candidates)
    (prefix . "^[\t ]*/// *<reference +path=\"\\(?:[^\"\n]*/\\)?\\([^/\"\n]*\\)")
    (requires . 0)
    (action . (lambda ()
                (when (string= (format "%c" (char-before)) "/")
                  (auto-complete '(ac-source-tss-referenc-path)))))))

(defvar ac-sources-tss
  '(ac-source-tss-member
    ac-source-tss-type
    ac-source-tss-new
    ac-source-tss-extends
    ac-source-tss-implements
    ac-source-tss-tag
    ac-source-tss-anything
    ac-source-tss-keyword
    ac-source-tss-special-comment
    ac-source-tss-special-comment-attr
    ac-source-tss-referenc-path))

(defvar-local tss--last-ac-start-point 1)
(defvar-local tss--last-ac-candidates nil)

(defun tss--insert-with-ac-trigger-command (n)
  (interactive "p")
  (self-insert-command n)
  (auto-complete-1 :triggered 'trigger-key))

(defun tss--get-active-code-prefix (re)
  (when (and (tss--active-code-point-p)
             (re-search-backward (concat re "\\=") nil t))
    (or (match-beginning 1)
        (match-beginning 0))))

(defun tss--get-ac-special-comment-candidates ()
  (loop for e in tss--builtin-special-comments
        collect (car e)))

(defun tss--get-ac-special-comment-attribute-candidates ()
  (let ((com (save-excursion
               (goto-char (point-at-bol))
               (when (re-search-forward "\\=[\t ]*/// *<\\([a-zA-Z0-9:-]+\\)" nil t)
                 (match-string-no-properties 1)))))
    (when com
      (assoc-default com tss--builtin-special-comments))))

(defun tss--get-ac-reference-path-candidates ()
  (save-excursion
    (re-search-backward "[^/\"\n]+\\=" nil t)
    (let* ((currpt (point))
           (prevpath (or (when (search-backward "\"" nil t)
                           (forward-char 1)
                           (buffer-substring-no-properties (point) currpt))
                         ""))
           (dirpath (concat (file-name-directory (expand-file-name (buffer-file-name)))
                            prevpath)))
      (loop for e in (directory-files dirpath)
            for fullpath = (concat dirpath e)
            if (and (file-regular-p fullpath)
                    (string-match "\\.ts\\'" e))
            collect e
            else if (and (file-directory-p fullpath)
                         (or (string= prevpath "")
                             (string-match "\\`[./]+/\\'" prevpath)
                             (not (member e '("." "..")))))
            collect (concat e "/")
            else if (file-symlink-p fullpath)
            collect e))))

(defun tss--get-ac-member-candidates ()
  (tss--trace "start get ac member candidates.")
  (tss--get-ac-candidates t))

(defun tss--get-ac-non-member-candidates ()
  (tss--trace "start get ac non member candidates.")
  (tss--get-ac-candidates nil))

(defun tss--get-ac-candidates (memberp)
  (yaxception:$
    (yaxception:try
      (when (> tss--last-ac-start-point (point-max))
        (setq tss--last-ac-start-point (point-max)))
      (let* ((currpt (point))
             (code (buffer-substring-no-properties currpt tss--last-ac-start-point)))
        (if (and (> currpt tss--last-ac-start-point)
                 (string-match "\\`[a-zA-Z0-9_]+\\'" code))
            (progn (tss--trace "Use last ac candidates. code[%s]" code)
                   tss--last-ac-candidates)
          (tss--trace "Start get ac candidates. code[%s]" code)
          (setq tss--last-ac-start-point currpt)
          (setq tss--last-ac-candidates
                (when (tss--sync-server)
                  (let* ((posarg (tss--get-position-argument))
                         (memberarg (cond (memberp "true")
                                          (t       "false")))
                         (fpath (expand-file-name (buffer-file-name)))
                         (cmdnm (cond (memberp "completions")
                                      (t       "completions-brief")))
                         (cmdstr (format "%s %s %s %s" cmdnm memberarg posarg fpath))
                         (ret (tss--get-server-response cmdstr :waitsec 2))
                         (entries (when (listp ret)
                                    (cdr (assoc 'entries ret)))))
                    (mapcar (lambda (e)
                              (let ((name (cdr (assoc 'name e)))
                                    (kind (cdr (assoc 'kind e)))
                                    (type (cdr (assoc 'type e)))
                                    (doc  (cdr (assoc 'docComment e))))
                                (tss--debug "Got candidate name[%s] kind[%s] type[%s]" name kind type)
                                (popup-make-item
                                 name
                                 :symbol (tss--get-ac-symbol kind)
                                 :summary (tss--get-ac-summary type)
                                 :document `(lambda (sel) (tss--get-ac-document ,name ,kind ,type ,doc)))))
                            entries)))))))
    (yaxception:catch 'error e
      (tss--show-message "%s" (yaxception:get-text e))
      (tss--error "failed get ac candidates : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e))
      (setq tss--last-ac-candidates nil))))

(defun tss--get-ac-keyword-candidates ()
  (tss--trace "start get ac keyword candidates.")
  (mapcar (lambda (s)
            (tss--debug "Got candidate name[%s] kind[builtin-keyword] type[]" s)
            (propertize s 'tss--ac-cand-kind "builtin-keyword"))
          tss--builtin-keywords))

(defun tss--get-ac-symbol (kind)
  (let ((kind (tss--stringify-response-element kind)))
    (cond ((member kind '("keyword" "builtin-keyword"))  "w")
          ((string= kind "primitive type")               "p")
          ((string= kind "module")                       "m")
          ((string= kind "interface")                    "i")
          ((string= kind "class")                        "c")
          ((member kind '("var" "property" "parameter")) "v")
          ((tss--function-kind-p kind)                   "f")
          ((string= kind "unknown")                      "")
          (t
           (tss--warn "found unknown server response for kind : %s" kind)
           ""))))

(defun tss--get-ac-summary (sum)
  (when (stringp sum)
    (let ((str (replace-regexp-in-string "\r?\n" " " sum)))
      (truncate-string-to-width
       str tss-ac-summary-truncate-length 0 nil "..."))))

(defun tss--get-ac-document (name kind type doc)
  (let* ((sym (intern (tss--get-ac-symbol kind)))
         (kind (tss--stringify-response-element kind))
         (type (tss--stringify-response-element type))
         (doc (or doc ""))
         (typedesc (case sym
                     (w "")
                     (f (concat "Signature: " type "\n\n"))
                     (t (concat "Type: " (capitalize type) "\n\n")))))
    (concat name " is " (upcase kind) ".\n\n" typedesc doc "\n")))


;;;;;;;;;;;;;;;;;;
;; For eldoc.el

(defvar-local tss--last-method-eldoc "")
(defvar-local tss--last-param-startpt 0)
(defvar-local tss--last-param-index -2)
(defvar-local tss--last-method-definition nil)

(defun tss--echo-method-usage ()
  (yaxception:$
    (yaxception:try
      (when (tss--active-p)
        (let* ((currpt (point))
               (parampt (tss--get-current-param-startpt))
               (arg-text (when parampt
                           (tss--remove-brace-part
                            (tss--buffer-substring-in-code parampt currpt))))
               (paramidx (when arg-text (length (split-string arg-text ",")))))
          (princ (cond ((not parampt)
                        "")
                       ((and (eq parampt tss--last-param-startpt)
                             (eq paramidx tss--last-param-index))
                        tss--last-method-eldoc)
                       ((and (eq parampt tss--last-param-startpt)
                             tss--last-method-definition)
                        (setq tss--last-param-index paramidx)
                        (setq tss--last-method-eldoc
                              (tss--build-method-eldoc tss--last-method-definition paramidx)))
                       (t
                        (setq tss--last-param-startpt parampt)
                        (setq tss--last-param-index paramidx)
                        (setq tss--last-method-definition
                              (when (tss--sync-server)
                                (let* ((posarg (save-excursion
                                                 (goto-char parampt)
                                                 (tss--get-position-argument)))
                                       (fpath (expand-file-name (buffer-file-name)))
                                       (cmdstr (format "type %s %s" posarg fpath))
                                       (ret (tss--get-server-response cmdstr :waitsec 1))
                                       (name (when (listp ret) (cdr (assoc 'fullSymbolName ret))))
                                       (kind (when (listp ret) (cdr (assoc 'kind ret))))
                                       (type (when (listp ret) (cdr (assoc 'type ret)))))
                                  (if (tss--function-kind-p (format "%s" kind))
                                      (tss--build-method-definition name type)
                                    ""))))
                        (setq tss--last-method-eldoc
                              (tss--build-method-eldoc tss--last-method-definition paramidx))))))))
    (yaxception:catch 'error e
      (tss--error "failed echo method usage : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (setq tss--last-method-definition "")
      (setq tss--last-method-eldoc "")
      (princ ""))))

(defvar tss--regexp-method-signature (rx-to-string `(and bos
                                                         (group (+ (not (any "("))))
                                                         "(" (group (+ anything)) ")"
                                                         (group (+ (not (any ")"))))
                                                         eos)))
(defun tss--build-method-eldoc (def highlight-index)
  (tss--trace "start build method eldoc. def[%s] highlight-index[%s]" def highlight-index)
  (when (not (facep 'tss--eldoc-type-highlight-face))
    (make-face 'tss--eldoc-type-highlight-face)
    (copy-face 'tss-eldoc-type-face 'tss--eldoc-type-highlight-face)
    (set-face-bold-p 'tss--eldoc-type-highlight-face t))
  (if (not (string-match tss--regexp-method-signature def))
      def
    (let* ((name (match-string 1 def))
           (param (match-string 2 def))
           (ret (match-string 3 def)))
      (loop with startidx = 0
            with endidx = (length param)
            with tstartidx = 0
            with curridx = 1
            with chidx = 0
            with not-type-char-p = (lambda (ch chk)
                                     (and (string= ch chk)
                                          (not (eq (get-text-property chidx 'face param)
                                                   'tss-eldoc-type-face))))
            with update-face = (lambda ()
                                 (when (and (= curridx highlight-index)
                                            (> tstartidx startidx)
                                            (> chidx tstartidx))
                                   (put-text-property startidx tstartidx 'face 'bold param)
                                   (put-text-property tstartidx chidx 'face 'tss--eldoc-type-highlight-face param)
                                   t))
            while (< chidx endidx)
            for ch = (substring param chidx (+ chidx 1))
            if (funcall not-type-char-p ch ":")
            do (setq tstartidx (+ chidx 1))
            if (funcall not-type-char-p ch ",")
            do (progn
                 (when (funcall update-face) (setq chidx endidx))
                 (setq startidx (+ chidx 1))
                 (incf curridx))
            do (incf chidx)
            finally do (funcall update-face))
      (concat name "(" param ")" ret))))


;;;;;;;;;;;;;;;;;;;
;; For implement

(defvar tss--regexp-component-definition
  (rx-to-string `(and bol (* space)
                      (? "export" (+ space))
                      (? (or "public" "private") (+ space))
                      (group (or "class" "interface")) (+ space)
                      (group (+ (any "a-zA-Z0-9_."))))))

(defun tss--get-implementable-constructor-definition (ident code)
  (let* ((buff (tss--make-temp-buffer
                (concat code "\nvar _xxx = new " ident "()")))
         (posarg (with-current-buffer buff
                   (forward-char -3)
                   (tss--get-position-argument)))
         (fpath (expand-file-name (buffer-file-name)))
         (cmdstr (format "type %s %s" posarg fpath))
         (ret (when (tss--sync-server :buff buff :path fpath)
                (tss--get-server-response cmdstr)))
         (type (when (and ret (listp ret))
                 (cdr (assoc 'type ret)))))
    (when type
      (list (replace-regexp-in-string
             ":[^:]+\\'" "" (tss--build-method-definition "constructor" type t))))))

(defun tss--get-implementable-method-definition (ident code)
  (let* ((buff (tss--make-temp-buffer
                (concat code "\nvar _xxx: " ident "; _xxx.")))
         (posarg (with-current-buffer buff
                   (tss--get-position-argument)))
         (fpath (expand-file-name (buffer-file-name)))
         (cmdstr (format "completions true %s %s" posarg fpath))
         (ret (when (tss--sync-server :buff buff :path fpath)
                (tss--get-server-response cmdstr)))
         (entries (when (and ret (listp ret))
                    (cdr (assoc 'entries ret)))))
    (loop for e in (mapcar (lambda (e)
                             (let ((name (cdr (assoc 'name e)))
                                   (kind (cdr (assoc 'kind e)))
                                   (type (cdr (assoc 'type e))))
                               (when (tss--function-kind-p (tss--stringify-response-element kind))
                                 (tss--build-method-definition name type t))))
                           entries)
          if e collect e)))

(defun tss--select-implementable-definition (cands)
  (let* ((cands (append (list "all") cands))
         (src `((name . "Implementable Definition")
                (candidates . ,cands)
                (candidate-number-limit . 999)
                (action . (lambda (cand)
                            (or (when (fboundp 'helm-marked-candidates) (helm-marked-candidates))
                                (when (fboundp 'anything-marked-candidates) (anything-marked-candidates))
                                (list cand)))))))
    (cond ((fboundp 'helm)
           (helm :sources src))
          ((fboundp 'anything)
           (anything :sources src))
          (t
           (list (completing-read "Implementable Definition" cands nil t nil '() "all"))))))

(defun tss--write-implementable-definition (defs classp)
  (let ((suffix (if classp " {}" ";")))
    (dolist (d defs)
      (newline-and-indent)
      (insert d suffix))))


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun tss-popup-help ()
  "Popup help about anything at point."
  (interactive)
  (yaxception:$
    (yaxception:try
      (when (tss--sync-server)
        (let* ((posarg (tss--get-position-argument))
               (fpath (expand-file-name (buffer-file-name)))
               (cmdstr (format "type %s %s" posarg fpath))
               (ret (tss--get-server-response cmdstr))
               (name (when (listp ret) (cdr (assoc 'fullSymbolName ret))))
               (kind (when (listp ret) (cdr (assoc 'kind ret))))
               (type (when (listp ret) (cdr (assoc 'type ret))))
               (doc (when (listp ret) (cdr (assoc 'docComment ret)))))
          (tss--popup-tip (tss--get-ac-document name kind type doc)))))
    (yaxception:catch 'error e
      (tss--show-message "%s" (yaxception:get-text e))
      (tss--error "failed popup help : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun tss-jump-to-definition ()
  "Jump to method definition at point."
  (interactive)
  (yaxception:$
    (yaxception:try
      (when (tss--sync-server)
        (let* ((posarg (tss--get-position-argument))
               (fpath (expand-file-name (buffer-file-name)))
               (cmdstr (format "definition %s %s" posarg fpath))
               (ret (tss--get-server-response cmdstr))
               (deffile (when (listp ret) (cdr (assoc 'file ret))))
               (startloc (when (listp ret) (cdr (assoc 'min ret))))
               (startrow (cond ((vectorp startloc) (elt startloc 0))
                               ((listp startloc)   (cdr (assoc 'line startloc)))))
               (startcol (cond ((vectorp startloc) (elt startloc 1))
                               ((listp startloc)   (cdr (assoc 'character startloc))))))
          (if (or (not deffile)
                  (not (file-exists-p deffile))
                  (not startrow)
                  (not startcol))
              (progn (tss--show-message "Not found definition location at point")
                     (tss--trace "Not found location file[%s] row[%s] col[%s]" deffile startrow startcol))
            (ring-insert find-tag-marker-ring (point-marker))
            (find-file deffile)
            (goto-char (point-min))
            (forward-line (- startrow 1))
            (forward-char (- startcol 1))))))
    (yaxception:catch 'error e
      (tss--show-message "%s" (yaxception:get-text e))
      (tss--error "failed jump to definition : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun tss-implement-definition ()
  "Implement inherited definitions of current component."
  (interactive)
  (yaxception:$
    (yaxception:try
      (let* (imples classp
             (pt (save-excursion
                   (when (re-search-backward tss--regexp-component-definition nil t)
                     (setq classp (string= (match-string-no-properties 1) "class"))
                     (goto-char (match-end 0))
                     (skip-syntax-forward " ")
                     (when (re-search-forward "\\=extends +\\([a-zA-Z0-9_.]+\\)" nil t)
                       (when classp
                         (push `(constructor . ,(match-string-no-properties 1)) imples))
                       (push `(method . ,(match-string-no-properties 1)) imples))
                     (skip-syntax-forward " ")
                     (when (re-search-forward "\\=implements +\\([a-zA-Z0-9_.]+\\)" nil t)
                       (push `(method . ,(match-string-no-properties 1)) imples))
                     (tss--search-forward-in-code "{" nil))))
             (code (when (and imples pt)
                     (buffer-substring-no-properties (point-min) pt)))
             (cands (when code
                      (loop for e in (reverse imples)
                            for what = (car e)
                            for ident = (cdr e)
                            if (eq what 'constructor)
                            append (tss--get-implementable-constructor-definition ident code)
                            if (eq what 'method)
                            append (tss--get-implementable-method-definition ident code)))))
        (cond ((not pt)
               (tss--show-message "Not implementable component location"))
              ((not cands)
               (tss--show-message "Not found implementable definition"))
              (t
               (tss--awhen (tss--select-implementable-definition cands)
                 (goto-char pt)
                 (tss--write-implementable-definition (if (member "all" it) cands it)
                                                      classp))))))
    (yaxception:catch 'error e
      (tss--show-message "%s" (yaxception:get-text e))
      (tss--error "failed implement definition : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e)))))

(defcustom tss-idle-task-idle-interval 2
  "Idle interval to run `tss-idle-task' in the background."
  :type 'integer
  :group 'tss)

(defvar tss-idle-task-timer nil
  "TSS Idle timer for various tasks: flymake, status line update
and etc.")

;; TODO flymake sometimes are slow and easy to break TSS
(defcustom tss-run-flymake-idlep nil
  "Whether to run flymake when idle.")

(defun tss-idle-task ()
  "Idle tasks registered with `tss-idle-timer'."
  (when (tss--active-p)
    (tss--set-status-mode-line-str)
    (if (tss--exist-process)
        (when tss-run-flymake-idlep
          (tss-run-flymake))
      ;; try to (soft)-restart TSS current buffer
      (tss-restart-current-buffer)
      (unless (tss--exist-process)
        (error "TSS idle soft restart failed!!!")))))

;;; TODO migrate to modern framework: flycheck
;;;###autoload
(defun tss-run-flymake ()
  "Run check by flymake for current buffer.

Always perform this checking over selected window buffer"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (and (tss--active-p)
               (tss--exist-process))
      (tss--debug "Start run flymake")
      (tss--sync-server)
      (setq flymake-last-change-time nil)
      (setq flymake-check-start-time (if (functionp 'flymake-float-time)
                                         (flymake-float-time)
                                       (float-time)))
      (let* ((ret (tss--get-server-response "showErrors"
                                            :waitsec 2
                                            :response-start-char "["
                                            :response-end-char "]"))
             (errors (when (arrayp ret)
                       (mapcar (lambda (e)
                                 (let* ((file (cdr (assoc 'file e)))
                                        (start (cdr (assoc 'start e)))
                                        (line (cdr (assoc 'line start)))
                                        (col (cdr (or (assoc 'character start)
                                                      (assoc 'col start))))
                                        (text (cdr (assoc 'text e))))
                                   (tss--trace "Found error file[%s] line[%s] col[%s] ... %s" file line col text)
                                   (format "%s (%d,%d): %s" file (or line 0) (or col 0) text)))
                               ret))))
        (when errors
          (setq flymake-new-err-info (flymake-parse-err-lines flymake-new-err-info errors)))
        (flymake-post-syntax-check 0 "tss")
        (setq flymake-is-running nil)))))

;;; TODO use "files" command and `get-file-buffer' to reload the whole project.
;;;###autoload
(defun tss-reload-current-project ()
  "Reload project data for current buffer."
  (interactive)
  (error "Reloading project is NOT supported."))

;;;###autoload
(defun tss-restart-current-buffer (&optional hardp)
  "Restart TSS for current buffer, possibly starting a new TS
service.

If there is not TS project for current buffer, always start a new
TS service.

If a project containing this file is alive, reload all setup from
project buffer. Only when HARDP is non-nil, start a new TS
service as well. "
  (interactive "P")
  ;; dynamically get new project, in case this file has recently been
  ;; included into a project.
  (let ((fpath (buffer-file-name)))
    (setq tss--project (tss--get-project fpath))
    (if (s-equals? fpath tss--project)
        ;; no project, always restart
        (progn
          (setq tss--last-send-string-failed-p nil)
          (setq tss--current-active-p t)
          (setq tss--project-buffer (current-buffer))
          (setq tss--proc (tss--start-process :projectp nil
                                              :initializep t)))
      ;; hard restart
      (when hardp
        (remhash tss--project tss--project-runtime-table))
      (tss--setup-project-for-current-buffer tss--project)))
  (tss--set-status-mode-line-str))

;;;###autoload
(defun tss-stop-current-buffer ()
  "Stop TSS for current buffer."
  (interactive)
  (setq tss--current-active-p nil)
  (when (tss--delete-process nil t)
    (tss--show-message "Stopped '%s'." (buffer-name))))

(defcustom tss-completion-engine 'auto-complete
  "The TSS completion enabled by `tss-setup-current-buffer'."
  :type 'symbol
  :group 'tss)

;;;###autoload
(defsubst tss-setup-completion-engine ()
  "Setup proper completion engine for tss buffers"
  (pcase tss-completion-engine
    (`auto-complete
     (loop for mode in tss-enable-modes
           do (add-to-list 'ac-modes mode))
     ;; auto trigger key (buffer local)
     (loop for stroke in tss-ac-trigger-command-keys
              if (not (string= stroke ""))
              do (local-set-key (read-kbd-macro stroke)
                                'tss--insert-with-ac-trigger-command))
     (loop for source in (reverse ac-sources-tss)
           do (add-to-list 'ac-sources source))
     (auto-complete-mode t)
     (eldoc-add-command 'tss--insert-with-ac-trigger-command))
    (t
     ;; other types of completion, some of them can setup themselves.
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: TS Project
;;;
(defvar tss--root-sources-project-configs-table (make-hash-table :test #'equal)
  "A TypeScript project can either be defined by \"tsconfig.js\"
or root-sources files. This variable is a table mapping from
project root (absolute) to a list root sources (absolute).

The key and value of this table need to be canonized path, DO NOT
use `puthash' directly. Use `tss-project-configure' instead.

*NOTE*: Currently manual setting is required for a new project.

TODO gulp config file may be parsed to get automatic root source configurations.
TODO implement support for tsconfig.js.")

(defun tss--get-project (fpath)
  "Get the project path for FPATH. Project configurations is
retrieved from `tss--root-sources-project-configs-table'. In case
no project exists for FPATH, FPATH is returned."
  (or (loop for root in (hash-table-keys tss--root-sources-project-configs-table)
            when (string-prefix-p root fpath)
            return root)
      ;; one-file virtual project ;P
      fpath))

(defun tss--setup-project-for-current-buffer (prjroot)
  "Setup project for current buffer.

Under a project:
1. look up `tss--project-runtime-table' to see whether this
project has already been setup if yes return all related
settings.
2. If not, start such a project setup."
  (let ((prjbuf (gethash prjroot tss--project-runtime-table)))
    (if (buffer-live-p prjbuf)
        (progn
          (setq tss--project-buffer prjbuf)
          (setq tss--proc (buffer-local-value 'tss--proc prjbuf)))
      ;; create new setup, project buffer is a virtual buffer (ps: buffer is
      ;; ELisp's object)
      (setq tss--project-buffer
            (get-buffer-create
             ;; buffer name is only suggestive
             (format " *TS: %s*" (file-name-nondirectory tss--project))))
      (with-current-buffer tss--project-buffer
        (setq default-directory prjroot)
        (setq tss--project prjroot)
        (setq tss--project-buffer (current-buffer))
        (setq tss--proc (tss--start-process :projectp t
                                            :initializep t)))
      (puthash prjroot tss--project-buffer tss--project-runtime-table)
      ;; recursive call to setup current buffer
      (tss--setup-project-for-current-buffer prjroot))))

(defun tss-project-configure (prjroot root-sources)
  "Set ROOT-SOURCES for PRJROOT in
`tss--root-sources-project-configs-table'. Always use this
function to setup TS projects.

ROOT-SOURCES can be relative to PRJROOT, which itself needs to be
+absolute (TODO eliminate this limit)."
  (let ((prjroot (expand-file-name prjroot)))
    (puthash prjroot
             (loop for path in root-sources
                   collect (expand-file-name path prjroot))
             tss--root-sources-project-configs-table)))

(defvar-local tss--status-mode-line-str ""
  "Mode line string to indicate the current status of TSS process
associated with this buffer.")
(put 'tss--status-mode-line-str 'risky-local-variable t)

(defun tss--set-status-mode-line-str ()
  "Return an appropriate string based on `tss--proc's status. "
  (when (tss--active-p)
    (let ((status (process-status tss--proc))
          (tss-str "TSS"))
      (setq tss-str
            (pcase status
              (`run
               (propertize tss-str 'face 'hi-green-b))
              (_
               (propertize tss-str 'face 'error))))
      (setq tss--status-mode-line-str (s-concat "[" tss-str "] ")))))


;;;###autoload
(defun tss-setup-current-buffer ()
  "Do setup for using TSS in current buffer."
  (interactive)
  (let ((fpath (buffer-file-name)))
    ;; no TSS for buffer with no file associated
    (when (and fpath
               (tss--active-p))
      ;; Key binding
      (loop for e in '((tss-popup-help-key . tss-popup-help)
                       (tss-jump-to-definition-key . tss-jump-to-definition)
                       (tss-implement-definition-key . tss-implement-definition))
            for key = (symbol-value (car e))
            for command = (cdr e)
            if (and (stringp key)
                    (not (string= key "")))
            do (local-set-key (read-kbd-macro key) command))
      (when (and (stringp tss-jump-to-definition-key)
                 (not (string= tss-jump-to-definition-key "")))
        (local-set-key (read-kbd-macro tss-jump-to-definition-key) 'tss-jump-to-definition))
      ;; For completions
      (tss-setup-completion-engine)
      ;; For eldoc
      (set (make-local-variable 'eldoc-documentation-function) 'tss--echo-method-usage)
      (turn-on-eldoc-mode)
      (when (commandp 'typescript-insert-and-indent)
        (eldoc-add-command 'typescript-insert-and-indent))
      ;; For flymake
      (flymake-mode t)
      ;; Setup TSS
      (tss-restart-current-buffer t)
      (tss--info "finished setup for %s" (current-buffer))
      ;; setup mode-line
      (tss--set-status-mode-line-str))))

;;;###autoload
(defun tss-config-default ()
  "Do setting recommemded configuration."
  ;; Activate auto-complete and setup TSS automatically when open tss-enable-modes buffer.
  (loop for mode in tss-enable-modes
        for hook = (intern-soft (concat (symbol-name mode) "-hook"))
        if (and hook
                (symbolp hook))
        do (add-hook hook 'tss-setup-current-buffer t))
  ;; Run flymake when save buffer.
  (add-to-list 'flymake-err-line-patterns '("\\`\\(.+?\\.ts\\) (\\([0-9]+\\),\\([0-9]+\\)): \\(.+\\)" 1 2 3 4))
  (add-hook 'after-save-hook 'tss-run-flymake t)
  ;; mode line indicator
  (add-to-list 'mode-line-modes '(t tss--status-mode-line-str))
  ;; idle timer to maintain various meta info or background checking.
  (unless tss-idle-task-timer
    (setq tss-idle-task-timer
          (run-with-idle-timer tss-idle-task-idle-interval
                               t #'tss-idle-task))))


(provide 'tss)
;;; tss.el ends here
