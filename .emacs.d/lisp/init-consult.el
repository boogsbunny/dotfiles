;;--------------------------------------------------------------------
;; consult
;;--------------------------------------------------------------------

(require 'seq)
(require 'patch-consult-comint)
(require 'consult)
(require 'consult-imenu)

(defun consult-ripgrep-up-directory ()
  (interactive)
  (let ((parent-dir (file-name-directory
                     (directory-file-name default-directory))))
    (when parent-dir
        (run-at-time 0 nil
                     #'consult-ripgrep
                     parent-dir
                     (ignore-errors
                       (buffer-substring-no-properties
                        (1+ (minibuffer-prompt-end)) (point-max))))))
  (minibuffer-quit-recursive-edit))

(consult-customize
 consult-ripgrep
 :keymap (let ((map (make-sparse-keymap)))
           (define-key map (kbd "M-l") #'consult-ripgrep-up-directory)
           map))

(consult-customize
 consult-bookmark
 consult-buffer
 consult-git-grep
 consult-grep
 consult-man
 consult-recent-file
 consult-ripgrep
 consult-xref
 :preview-key '(:debounce 0.2 any))

(defun boogs/consult-grep-git-or-rg (arg)
  (interactive "P")
  (require 'vc)
  (require 'functions)
  (if (and (vc-find-root default-directory ".git")
           (or arg
               (split-string
                (boogs/call-process-to-string "git" "ls-files" "-z")
                "\0"
                t)))
      (consult-ripgrep arg)
    (consult-ripgrep)))

(setq consult-async-min-input 3)
(setq consult-async-input-debounce 0.5)
(setq consult-async-input-throttle 0.8)

(setq consult-find-args
      (concat "find . -not ( "
              "-path */.git* -prune "
              "-or -path */.cache* -prune )"))

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(define-key minibuffer-mode-map (kbd "M-p") 'consult-history)

(defvar-local consult-toggle-preview-orig nil)

(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (progn
        (setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
        (message "Live preview is enabled"))
    (progn
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)
      (message "Live preview is disabled"))))

(define-key vertico-map (kbd "C-c C-f") #'consult-toggle-preview)

(defun boogs/consult--name-allowed-p (name)
  (not (seq-some (lambda (rx) (string-match-p rx name))
                 consult-buffer-filter)))

(defun boogs/consult--not-exwm-buffer-p (buf)
  (and (buffer-live-p buf)
       (not (eq (buffer-local-value 'major-mode buf) 'exwm-mode))))

(defun boogs/consult-perspective-items ()
  "Current perspective buffer names (EXWM excluded, name-filtered)."
  (let* ((bufs (cond
                ((and (fboundp 'persp-buffers) (fboundp 'persp-curr))
                 (ignore-errors (persp-buffers (persp-curr))))
                ((fboundp 'persp-get-buffer-names)
                 (mapcar #'get-buffer (persp-get-buffer-names)))
                (t nil))))
    (->> bufs
         (seq-filter #'boogs/consult--not-exwm-buffer-p)
         (seq-map #'buffer-name)
         (seq-filter #'boogs/consult--name-allowed-p))))

(defvar boogs/consult-source-perspective
  `(:name "Perspective"
    :narrow ?s
    :category buffer
    :face consult-buffer
    :default t
    ;; :action ,#'consult--buffer-action
    :state ,#'consult--buffer-state
    :items ,#'boogs/consult-perspective-items)
  "Current perspective buffers only (EXWM excluded).")

(defun boogs/consult-exwm-items ()
  (->> (buffer-list)
       (seq-filter (lambda (b)
                     (eq (buffer-local-value 'major-mode b)
                         'exwm-mode)))
       (seq-map #'buffer-name)
       (seq-filter #'boogs/consult--name-allowed-p)))

(defvar boogs/consult-source-exwm
  `(:name "EXWM"
    :narrow ?x
    :category buffer
    :face consult-buffer
    ;; :action ,#'consult--buffer-action
    :state ,#'consult--buffer-state
    :items ,#'boogs/consult-exwm-items)
  "EXWM buffers in a separate group, no preview.")

(when (fboundp 'persp-mode)
  (setq consult-buffer-sources '(boogs/consult-source-perspective
                                 boogs/consult-source-exwm)))

(consult-customize consult--source-buffer :hidden t :default nil)

(defcustom boogs/consult-ripgrep-or-line-limit 300000
  "Buffer size threshold for `boogs/consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
  :type 'integer)

(defun boogs/consult-ripgrep-or-line ()
  "Call `consult-line' for small buffers or `consult-ripgrep' for
large files."
  (interactive)
  (if (or (not buffer-file-name)
          (buffer-narrowed-p)
          (ignore-errors
            (file-remote-p buffer-file-name))
          (jka-compr-get-compression-info buffer-file-name)
          (<= (buffer-size)
              (/ boogs/consult-ripgrep-or-line-limit
                 (if (eq major-mode 'org-mode) 4 1))))
      (consult-line)
    (when (file-writable-p buffer-file-name)
      (save-buffer))
    (let ((consult-ripgrep-args
           (concat consult-ripgrep-args
                   ;; filter to desired filename
                   " -g "
                   (shell-quote-argument
                    (file-name-nondirectory buffer-file-name))
                   " ")))
      (consult-ripgrep))))

(defun boogs/consult-find-file-with-preview (prompt &optional
                                                    dir
                                                    default
                                                    mustmatch
                                                    initial
                                                    pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal
                   :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

;; (setq read-file-name-function #'boogs/consult-find-file-with-preview)

(defun boogs/consult-project-find-file-with-preview (prompt all-files &optional pred hist _mb)
  (let ((prompt (if (and all-files
                         (file-name-absolute-p (car all-files)))
                    prompt
                  ( concat prompt
                    ( format " in %s"
                      (consult--fast-abbreviate-file-name default-directory)))))
        (minibuffer-completing-file-name t))
    (consult--read (mapcar
                    (lambda (file)
                      (file-relative-name file))
                    all-files)
                   :state (consult--file-preview)
                   :prompt (concat prompt ": ")
                   :require-match t
                   :history hist
                   :category 'file
                   :predicate pred)))

(setq project-read-file-name-function
      #'boogs/consult-project-find-file-with-preview)

;;; Comint
(defun boogs/comint-set-keys ()
  (define-key comint-mode-map (kbd "M-p") 'consult-history))

(add-hook 'comint-mode-hook 'boogs/comint-set-keys)

(provide 'init-consult)
