(defvar nsfwm-repl "nsfwm-repl")

(defun nsfwm-eval (sexp &optional target-buffer)
  "Pass SEXP to nsfwm for evaluation.

SEXP can either be a list or a string.

If given, the result of the evaluation is inserted into TARGET-BUFFER."
  (call-process nsfwm-repl nil target-buffer nil "-e"
                (if (stringp sexp) sexp (format "%S" sexp))))


(defun nsfwm-eval-region (start end &optional target-buffer)
  "Evaluate the region bounded by START and END.

TARGET-BUFFER is the optional target for the return value of the
evaluation."
  (interactive "r")
  (nsfwm-eval
   (concat "(begin "
           (buffer-substring-no-properties start end)
           ")")
   target-buffer))

(defun nsfwm-eval-buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (nswfwm-eval-region (point-min) (point-max) nil))

(provide 'nswfm)

;;; nsfwm.el ends here
