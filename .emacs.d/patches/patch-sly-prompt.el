(defun sly-mrepl--remove-comint-highlight (start end)
  "Some `comint' functions force the `comint-highlight-prompt'
face, which we don't want so we remove it."
  (font-lock--remove-face-from-text-property
   start end
   'font-lock-face 'comint-highlight-prompt))

(defun sly-mrepl--send-input-sexp ()
  (goto-char (point-max))
  (save-excursion
    (skip-chars-backward "\n\t\s")
    (delete-region (max (point)
                        (sly-mrepl--mark))
                   (point-max)))
  (buffer-disable-undo)
  (overlay-put sly-mrepl--last-prompt-overlay 'face 'highlight)
  (set (make-local-variable 'sly-mrepl--dirty-history) t)
  (let ((previous-comint-last-prompt comint-last-prompt))
    (sly-mrepl--commiting-text
        `(field sly-mrepl-input
                keymap ,(let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") 'sly-mrepl-insert-input)
                          (define-key map [return] 'sly-mrepl-insert-input)
                          (define-key map [mouse-2] 'sly-mrepl-insert-input)
                          map))
      (prog1 (comint-send-input)
        (sly-mrepl--remove-comint-highlight
         (car previous-comint-last-prompt)
         (cdr previous-comint-last-prompt))))))

(defun sly-mrepl--insert-prompt (package nickname error-level next-entry-idx
                                         &optional condition)
  (sly-mrepl--accept-process-output)
  (overlay-put sly-mrepl--last-prompt-overlay 'face 'bold)
  (when condition
    (sly-mrepl--insert-note (format "Debugger entered on %s" condition)))
  (sly-mrepl--ensure-newline)
  (sly-mrepl--catch-up)
  (let ((beg (marker-position (sly-mrepl--mark))))
    (sly-mrepl--insert
     (propertize
      (funcall sly-mrepl-prompt-formatter
               package
               nickname
               error-level
               next-entry-idx
               condition)
      'sly-mrepl--prompt (downcase package)))
    ;; `comint-output-filter' forces the `comint-highlight-prompt' face, which
    ;; we don't want, so we remove it here.
    (let ((prompt-start (save-excursion (forward-line 0) (point)))
          (inhibit-read-only t))
      (sly-mrepl--remove-comint-highlight prompt-start (point)))
    (move-overlay sly-mrepl--last-prompt-overlay beg (sly-mrepl--mark)))
  (buffer-disable-undo)
  (buffer-enable-undo))

(provide 'patch-sly-prompt)
