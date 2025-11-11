(require 'cl-lib)
(require 'consult)
(require 'comint)
(require 'subr-x)

(defgroup consult-comint nil
  "Consult utilities for comint buffers."
  :group 'consult
  :prefix "consult-comint-")

(defface consult-comint-index
  '((t :foreground "cyan"))
  "Face for the prompt index in prompt lists."
  :group 'consult-comint)

(defface consult-comint-buffer-name
  '((t :foreground "green"))
  "Face for buffer names in prompt lists."
  :group 'consult-comint)

(defcustom consult-comint-prompts-index-p t
  "When non-nil, show the prompt number in prompt pickers."
  :type 'boolean
  :group 'consult-comint)

(defcustom consult-comint-mode-list
  '(comint-mode shell-mode eshell-mode slime-repl-mode sly-mrepl-mode
                sql-interactive-mode)
  "Supported modes for prompt navigation/history.
Derived modes are also supported."
  :type '(repeat (choice symbol))
  :group 'consult-comint)

(defcustom consult-comint-next-prompt-function
  '((sly-mrepl-mode . (lambda () (sly-mrepl-next-prompt) (point))))
  "Alist of (MODE . NEXT-PROMPT-FN) returning point after next prompt.
If not present, fallback to `comint-next-prompt'."
  :type '(alist :key-type symbol :value-type function)
  :group 'consult-comint)

(defun consult-comint--in-supported-mode-p ()
  (or (apply #'derived-mode-p consult-comint-mode-list)
      (member major-mode consult-comint-mode-list)))

(defun consult-comint--replace-input (s)
  "Replace current comint input with S."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (delete-region (comint-line-beginning-position) (point-max))
    (goto-char (point-max))
    (insert s)))

;;;###autoload
(defun consult-comint-input-ring ()
  "Completion over `comint-input-ring' of the current buffer.
Candidates keep their original newlines (multiline). Selecting inserts
the entry at the prompt (does not execute)."
  (interactive)
  (unless (consult-comint--in-supported-mode-p)
    (user-error "Current buffer is not a comint buffer"))
  (unless (and (boundp 'comint-input-ring) (ring-p comint-input-ring))
    (user-error "No comint input ring in this buffer"))
  (let* ((initial (buffer-substring-no-properties
                   (comint-line-beginning-position) (point)))
         (raw (reverse (cl-remove-if #'string-empty-p
                                     (ring-elements comint-input-ring))))
         (alist (mapcar (lambda (s) (cons s s)) raw))
         (choice (consult--read (mapcar #'car alist)
                                :prompt "Comint history: "
                                :initial initial
                                :require-match t
                                :sort nil
                                :category 'consult-comint-history)))
    (when choice
      (consult-comint--replace-input (cdr (assoc choice alist))))))

(defun consult-comint--next-prompt-pos ()
  "Move to next prompt in current buffer and return point-after-prompt.
Uses `consult-comint-next-prompt-function' when available, else
`comint-next-prompt'. Returns nil when no more prompts."
  (let* ((fn (alist-get major-mode consult-comint-next-prompt-function))
         (pos (condition-case nil
                  (if fn
                      (funcall fn)
                    (progn (comint-next-prompt 1) (point)))
                (error nil))))
    pos))

(defun consult-comint--prompts-in-buffer (buffer)
  "Collect prompts in BUFFER.
Return list of (DISPLAY . (BUFFER MARKER INDEX)). DISPLAY contains the
prompt index (optional) and the first line of the prompt; jumping goes
to MARKER."
  (with-current-buffer buffer
    (when (consult-comint--in-supported-mode-p)
      (save-excursion
        (goto-char (point-min))
        (let ((idx 1)
              (acc nil))
          (while (< (point) (point-max))
            (let ((pos (consult-comint--next-prompt-pos)))
              (if (not pos)
                  (goto-char (point-max)) ; break
                (let* ((line (buffer-substring-no-properties
                              pos
                              (line-end-position)))
                       (m (copy-marker pos))
                       (idx-str (propertize (number-to-string idx)
                                            'face 'consult-comint-index))
                       (disp (if consult-comint-prompts-index-p
                                 (format "%s:%s" idx-str line)
                               line)))
                  (push (cons disp (list buffer m idx)) acc)
                  (setq idx (1+ idx))))))
          (nreverse acc))))))

(defun consult-comint--prompts-in-all-buffers (mode)
  "Collect prompts from all buffers derived from MODE.
Return list of (DISPLAY . (BUFFER MARKER INDEX))."
  (cl-loop for b in (buffer-list)
           nconc
           (with-current-buffer b
             (when (derived-mode-p mode)
               (let* ((items (consult-comint--prompts-in-buffer b))
                      (bufname (propertize (buffer-name b)
                                           'face 'consult-comint-buffer-name)))
                 (mapcar (lambda (it)
                           (pcase-let ((`(,disp . (,buf ,m ,idx)) it))
                             (cons (format "%s:%s"
                                           bufname
                                           (replace-regexp-in-string
                                            "^[0-9]+:"
                                            ""
                                            disp))
                                   (list buf m idx))))
                         items))))))

(defun consult-comint--goto (buffer marker where)
  "Jump to MARKER in BUFFER. WHERE is nil, 'window or 'frame."
  (pcase where
    ('window (switch-to-buffer-other-window buffer))
    ('frame  (switch-to-buffer-other-frame buffer))
    (_       (switch-to-buffer buffer)))
  (goto-char marker)
  (recenter))

(cl-defun consult-comint--prompt-read (candidates &key prompt where)
  "Read from CANDIDATES and jump.
CANDIDATES is an alist of (DISPLAY . (BUFFER MARKER INDEX))."
  (let* ((choice (consult--read (mapcar #'car candidates)
                                :prompt (or prompt "Comint prompts: ")
                                :require-match t
                                :sort nil
                                :category 'consult-location))
         (hit (and choice (assoc choice candidates))))
    (when hit
      (pcase-let ((`(,buf ,m ,_) (cdr hit)))
        (consult-comint--goto buf m where)))))

;;;###autoload
(defun consult-comint-prompts ()
  "Browse prompts in the current comint buffer and jump to one."
  (interactive)
  (unless (consult-comint--in-supported-mode-p)
    (user-error "Current buffer is not a comint buffer"))
  (let ((cands (consult-comint--prompts-in-buffer (current-buffer))))
    (if cands
        (consult-comint--prompt-read cands :prompt "Comint prompts: ")
      (message "No prompts found"))))

;;;###autoload
(defun consult-comint-prompts-all ()
  "Browse prompts across all comint buffers of the same family as the
current one."
  (interactive)
  (unless (consult-comint--in-supported-mode-p)
    (user-error "Current buffer is not a comint buffer"))
  (let* ((mode major-mode)
         (cands (consult-comint--prompts-in-all-buffers mode)))
    (if cands
        (consult-comint--prompt-read cands :prompt "All comint prompts: ")
      (message "No prompts found"))))

;;;###autoload
(defun consult-comint-prompts-other-window (&optional all)
  "Like `consult-comint-prompts', but jump in other window.
With prefix arg ALL, browse prompts across all comint buffers."
  (interactive "P")
  (if all
      (let* ((mode major-mode)
             (cands (consult-comint--prompts-in-all-buffers mode)))
        (if cands
            (consult-comint--prompt-read
             cands
             :prompt "All comint prompts (other window): "
             :where 'window)
          (message "No prompts found")))
    (let ((cands (consult-comint--prompts-in-buffer (current-buffer))))
      (if cands
          (consult-comint--prompt-read
           cands
           :prompt "Comint prompts (other window): "
           :where 'window)
        (message "No prompts found")))))

;;;###autoload
(defun consult-comint-prompts-other-frame (&optional all)
  "Like `consult-comint-prompts', but jump in other frame.
With prefix arg ALL, browse prompts across all comint buffers."
  (interactive "P")
  (if all
      (let* ((mode major-mode)
             (cands (consult-comint--prompts-in-all-buffers mode)))
        (if cands
            (consult-comint--prompt-read
             cands
             :prompt "All comint prompts (other frame): "
             :where 'frame)
          (message "No prompts found")))
    (let ((cands (consult-comint--prompts-in-buffer (current-buffer))))
      (if cands
          (consult-comint--prompt-read
           cands
           :prompt "Comint prompts (other frame): "
           :where 'frame)
        (message "No prompts found")))))

(provide 'patch-consult-comint)
