;;--------------------------------------------------------------------
;; default settings
;;--------------------------------------------------------------------

;; minimum version needed
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; system config
(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(if *sys/mac*
    (progn
      (setq mac-command-modifier 'meta) ; command = Meta
      (setq mac-option-modifier 'super) ; (left) option = Super
      (setq x-select-enable-clipboard 't)
      (toggle-frame-fullscreen)
      (setq exec-path (append '("/usr/local/bin") exec-path))
      (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
      (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
      (setq dired-use-ls-dired nil)))

;; encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; native comp settings
(setq native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors 'silent
      native-comp-async-query-on-exit t
      native-comp-async-jobs-number 4
      native-comp-speed 3)

;; avoid outdated byte-compiled elisp files
(setq load-prefer-newer t)

;;; Support for Emacs pinentry.
;;; Required for eshell/sudo and everything relying on GPG queries.
(setq epa-pinentry-mode 'loopback) ; This will fail if gpg >= 2.1 is not available.
(when (require 'pinentry nil t)
  (pinentry-start))

;; ignore bell
(setq ring-bell-function #'ignore)

;; Save copied text from outside emacs when yanking
(setq save-interprogram-paste-before-kill t)

;; make yes-or-no prompts y-or-n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Make cursor width of the character it is under
(setq x-stretch-cursor t)

;; follow symlinks
(setq vc-follow-symlinks t)

;; completion cycling
(setq completion-cycle-threshold t)

;; set tabs to 2 spaces
(setq-default tab-width 2)
;; (defvaralias 'standard-indent 'tab-width)
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; mode line
(display-time-mode)
(setq display-time-mode 1
      display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)


(setq rm-excluded-modes
      (mapconcat
       'identity
       ;; These names must start with a space!
       '(" GitGutter" " MRev" " company"
         " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
         " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
       "\\|"))

;; disable cursor blinking
(blink-cursor-mode 0)

;; highlight current line
(global-hl-line-mode 1)
;;(set-face-attribute hl-line-face nil :underline t)
(set-face-background 'hl-line nil)
(set-face-foreground 'hl-line nil)
(set-face-underline  'hl-line t)

;; highlight matching parenthesis
(show-paren-mode 1)
;; remove delay
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t)

;; show cursor position within line
(column-number-mode 1)

;; prettify symbols
(global-prettify-symbols-mode +1)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq whitespace-style '(face empty indentation space-after-tab space-before-tab tab-mark trailing))

;; remove the GUI Emacs clutter
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq use-dialog-box nil)

(setq recentf-max-saved-items 100
      scroll-step 1
      calendar-week-start-day 1
      calendar-date-style 'iso
      delete-by-moving-to-trash t
      uniquify-buffer-name-style 'forward
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-default nil
      auto-save-list-file-prefix nil
      split-height-threshold nil
      split-width-threshold 160)

;; remember last cursor position
(save-place-mode)
;; save if daemon is killed unexpectedly
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)
;;  save M-: history
(savehist-mode)

;; default mode
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
;; 80 character limit
(setq fci-rule-column 80)
;; spell check
(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
;; (add-hook 'text-mode-hook 'flyspell-mode)

;; disable lockfiles
(setq create-lockfiles nil)

;; properly delete tabs using backspace
(setq backward-delete-char-untabify-method 'hungry)

;; turn on line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type'relative)

(setq fill-column (string-to-number "140"))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(require 'auth-source-pass)
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
(add-to-list 'auth-sources 'password-store 'append)

;; disable some features and settings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; TODO
(defcustom boogs/buffer-skip-regexp
  (rx bos (or (or "*Backtrace*" "*Compile-Log*" "*Completions*"
                  "*Messages*" "*package*" "*Warnings*" "*Slack*"
                  "*Async-native-compile-log*")
              (seq "magit-diff" (zero-or-more anything))
              (seq "magit-process" (zero-or-more anything))
              (seq "magit-revision" (zero-or-more anything))
              (seq "magit-stash" (zero-or-more anything))
              (seq "helm" (zero-or-more anything)))
              eos)
  "Regular expression matching buffers ignored by `next-buffer' and
`previous-buffer'."
  :type 'regexp)

(defun boogs/buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `boogs/buffer-skip-regexp'."
  (string-match-p boogs/buffer-skip-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'boogs/buffer-skip-p)

(setq olivetti-body-width 140)
(add-hook 'text-mode-hook #'olivetti-mode)
(add-hook 'prog-mode-hook #'olivetti-mode)

;; kill this buffer
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
;; cycle buffers
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)

(defun guix-buffer-p (&optional buffer)
  (let ((buf-name (buffer-name (or buffer (current-buffer)))))
    (not (null (or (string-match "*Guix REPL" buf-name)
		   (string-match "*Guix Internal REPL" buf-name))))))

(defun guix-geiser--set-project (&optional _impl _prompt)
  (when (and (eq 'guile geiser-impl--implementation)
	     (null geiser-repl--project)
	     (guix-buffer-p))
    (geiser-repl--set-this-buffer-project 'guix)))

(advice-add 'geiser-impl--set-buffer-implementation :after #'guix-geiser--set-project)

(provide 'init-defaults)
