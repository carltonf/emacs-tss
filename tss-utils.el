;;; Various utility&helper functions

(defun tss-utils/alist-path (alist keys)
  "ALIST is a nested associate list, KEYS are a list of of keys.
Search KEYS in ALIST in a similar manner like XPATH. Return the
value after all KEYS found, or nil if some keys are not found."
  (loop for key in keys
        for nalist = (assoc key alist)
        if nalist
        do (setq alist (cdr nalist))
        else do (setq alist nil))
  alist)