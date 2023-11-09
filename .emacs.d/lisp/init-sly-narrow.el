;; requires `boogs/sly-end-of-prompt-p'.

(defun boogs/sly-prompt-begin-position ()
  (cond
   ((and (get-text-property (point) 'sly-mrepl--prompt)
         (not (get-text-property (1- (point)) 'sly-mrepl--prompt)))
    (point))
   ((or (get-text-property (point) 'sly-mrepl--prompt)
        (boogs/sly-end-of-prompt-p))
    (previous-single-char-property-change
     (point) 'sly-mrepl--prompt))
   (t (previous-single-char-property-change
       (previous-single-char-property-change
        (point) 'sly-mrepl--prompt)
       'sly-mrepl--prompt))))

(defun boogs/sly-prompt-end-position ()
  (save-excursion
    (goto-char (boogs/sly-prompt-begin-position))
    (call-interactively #'sly-mrepl-next-prompt)
    (point)))

(defun boogs/sly-output-end-position ()
  (if (get-text-property (point) 'sly-mrepl--prompt)
      (next-single-char-property-change
       (next-single-char-property-change
        (point) 'sly-mrepl--prompt)
       'sly-mrepl--prompt)
    (next-single-char-property-change
     (point) 'sly-mrepl--prompt)))

(defun boogs/sly-narrow-to-prompt ()
  "Narrow buffer to prompt at point."
  (interactive)
  (narrow-to-region
   (boogs/sly-prompt-begin-position)
   (boogs/sly-output-end-position)))

(with-eval-after-load 'sly-mrepl
  (define-key sly-mrepl-mode-map (kbd "C-x n d") 'boogs/sly-narrow-to-prompt))

(provide 'init-sly-narrow)
