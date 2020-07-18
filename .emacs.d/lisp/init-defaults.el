;;--------------------------------;
;; Default settings
;;--------------------------------;

;; minimum version needed
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; :TODOs
;; install notmuch
;; build slack properly
;; install ejira properly
;; setup authinfo

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
      (setq dired-use-ls-dired nil)
      ))

;; encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)


;; avoid outdated byte-compiled elisp files
(setq load-prefer-newer t)

;; support for emacs pinentry
;; (use-package pinentry
;;   :ensure t)
;; (setq epa-pinentry-mode 'loopback)
;; (when (require 'pinentry nil t)
;;   (pinentry-start))

;; ignore bell
(setq ring-bell-function #'ignore)

;; Save copied text from outside emacs when yanking
(setq save-interprogram-paste-before-kill t)

;; make yes-or-no prompts y-or-n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Make cursor width of the character it is under
(setq x-stretch-cursor t)

;; completion cycling
(setq completion-cycle-threshold t)

;; set tabs to 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

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

;; remove the GUI Emacs clutter
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq use-dialog-box nil)

(setq
 recentf-max-saved-items 100
 ;; line by line scrolling
 scroll-step 1
 calendar-week-start-day 1
 calendar-date-style 'iso
 delete-by-moving-to-trash t
 uniquify-buffer-name-style 'forward

 backup-directory-alist
 `(("." . ,(expand-file-name "backups" user-emacs-directory)))

 ;; disable auto save
 auto-save-default nil
 auto-save-list-file-prefix nil

 split-height-threshold nil
 split-width-threshold 140)

;; remember last cursor position
(save-place-mode)
;; save if daemon is killed unexpectedly
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)
;;  save M-: history
(savehist-mode)

;; default mode
(setq-default major-mode 'text-mode)

;; disable lockfiles
(setq create-lockfiles nil)

;; properly delete tabs using backspace
(setq backward-delete-char-untabify-method 'hungry)

;; turn on line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;; autofill
;; (when (getenv "MANWIDTH")
;;   (setq-default fill-column (string-to-number (getenv "MANWIDTH"))))

(setq-default fill-column (string-to-number "70"))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; window resizing
;; (global-set-key (kbd "C-x l") 'shrink-window-horizontally)
;; (global-set-key (kbd "C-x j") 'enlarge-window-horizontally)
;; (global-set-key (kbd "C-x k") 'shrink-window)
;; (global-set-key (kbd "C-x j") 'enlarge-window)

;; windmove mode
(when (fboundp 'windmove-default-keybindings)
  (global-set-keys
   "C-M-h" 'windmove-left
   "C-M-j" 'windmove-down
   "C-M-k" 'windmove-up
   "C-M-l" 'windmove-right
   "C-M-o" 'delete-other-windows
   "C-M-c" 'delete-window))

;; kill this buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; disable some features and settings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

(provide 'init-defaults)
