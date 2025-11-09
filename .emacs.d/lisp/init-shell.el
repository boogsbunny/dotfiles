;;--------------------------------------------------------------------
;; shell
;;--------------------------------------------------------------------

;;; Shared global history.
(defvar boogs/shell-history-global-ring nil
  "The history ring shared across shell sessions.")

(defun boogs/shell-use-global-history ()
  "Make shell history shared across different sessions."
  (unless boogs/shell-history-global-ring
    (when comint-input-ring-file-name
      (comint-read-input-ring))
    (setq boogs/shell-history-global-ring (or comint-input-ring (make-ring comint-input-ring-size))))
  (setq comint-input-ring boogs/shell-history-global-ring))

(defun boogs/shell-history-remove-duplicates ()
  (require 'functions) ; For `boogs/ring-delete-first-item-duplicates'.
  (boogs/ring-delete-first-item-duplicates comint-input-ring))

(defvar boogs/comint-input-history-ignore (concat "^" (regexp-opt '("#" " " "cd ")))
  "`comint-input-history-ignore' can only be customized globally
because `comint-read-input-ring' uses a temp buffer.")

(defun boogs/shell-remove-ignored-inputs-from-ring ()
  "Discard last command from history if it matches
`boogs/comint-input-history-ignore'."
  (unless (ring-empty-p comint-input-ring)
    (when (string-match boogs/comint-input-history-ignore
                        (ring-ref comint-input-ring 0))
      (ring-remove comint-input-ring 0))))

(defun boogs/shell-sync-input-ring (_)
  (boogs/shell-history-remove-duplicates)
  (boogs/shell-remove-ignored-inputs-from-ring)
  (comint-write-input-ring))

(defun boogs/shell-setup ()
  (setq comint-input-ring-file-name
        (expand-file-name "shell-history" user-emacs-directory))
  (boogs/shell-use-global-history)

  ;; Write history on every command, not just on exit.
  (add-hook 'comint-input-filter-functions 'boogs/shell-sync-input-ring nil t)

  ;; Only ending with '#' or '$' but seems slower:
  ;; (setq comint-prompt-regexp "^[^#$]*
  ;; [^#$]*[#$>] +")
  (setq comint-prompt-regexp "^[^#$%>]*
\[^#$%>]*[#$%>] +"))

(add-hook 'shell-mode-hook 'boogs/shell-setup)

(defun boogs/shell-prompt-begin-position ()
  ;; We need this convoluted function because `looking-at-p' does not work on
  ;; multiline regexps _and_ `re-search-backward' skips the current line.
  (save-excursion
    (let ((old-point (point)))
      (max
       (save-excursion
         ;; Right result if not on prompt.
         (call-interactively #'comint-previous-prompt)
         (re-search-backward comint-prompt-regexp)
         (point))
       (save-excursion
         ;; Right result if on first char after prompt.
         (re-search-backward comint-prompt-regexp)
         (point))
       (save-excursion
         ;; Right result if on prompt.
         (call-interactively #'comint-next-prompt)
         (re-search-backward comint-prompt-regexp)
         (if (<= (point) old-point)
             (point)
           (point-min)))))))

(defun boogs/shell-prompt-end-position ()
  (save-excursion
    (goto-char (boogs/shell-prompt-begin-position))
    (call-interactively #'comint-next-prompt)
    (point)))

(defun boogs/shell-prompt ()
  (buffer-substring-no-properties
   (boogs/shell-prompt-begin-position)
   (boogs/shell-prompt-end-position)))

(defun boogs/shell-propertize-prompt ()        ; Inspired by `shx--propertize-prompt'.
  "Add a mouseover timestamp to the last prompt."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (add-text-properties
     (save-excursion
       (re-search-backward comint-prompt-regexp nil :noerror)
       (point))
     (process-mark (get-buffer-process (current-buffer)))
     `(help-echo ,(format-time-string "%F %T")))))

(defun boogs/shell-send-input ()
  "Send or parse the input currently written at the prompt.
In normal circumstances this input is additionally filtered by
`shx-filter-input' via `comint-mode'."
  (interactive)
  (boogs/shell-propertize-prompt)
  (comint-send-input))
(define-key shell-mode-map (kbd "<return>") 'boogs/shell-send-input)

(defun boogs/shell-command-duration ()
  "Return duration of command at point in a `shell' buffer."
  (interactive)
  (let ((begin (ignore-errors (parse-time-string (get-text-property
                                                  (boogs/shell-prompt-begin-position)
                                                  'help-echo))))
        (end (parse-time-string (save-excursion
                                  (goto-char (boogs/shell-prompt-end-position))
                                  (call-interactively #'comint-next-prompt)
                                  (boogs/shell-prompt)))))
    (if begin
        (message "Command took %.0f seconds."
                 (- (float-time (apply 'encode-time end))
                    (float-time (apply 'encode-time begin))))
      (message "No timestamp."))))

(defun boogs/shell-narrow-to-prompt ()
  "Narrow buffer to prompt at point."
  (interactive)
  (let ((begin (boogs/shell-prompt-begin-position)))
    (narrow-to-region
     begin
     (save-excursion
       (goto-char (boogs/shell-prompt-end-position))
       (call-interactively #'comint-next-prompt)
       (if (= begin
              (boogs/shell-prompt-begin-position))
           (point-max)
         (boogs/shell-prompt-begin-position))))))
(define-key shell-mode-map (kbd "C-x n d") 'boogs/shell-narrow-to-prompt)

(define-key shell-mode-map (kbd "C-x M-O") 'comint-truncate-buffer)

(provide 'init-shell)
