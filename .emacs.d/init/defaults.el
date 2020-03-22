;;--------------------------------;
;; Default settings
;;--------------------------------;

;; encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; avoid outdated byte-compiled elisp files
(setq load-prefer-newer t)

;; support for emacs pinentry
(use-package pinentry
  :ensure t)
(setq epa-pinentry-mode 'loopback)
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

;; set tabs to 4 spaces
(setq-default tab-width 4)
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

;; show cursor position within line
(column-number-mode 1)

;; prettify symbols
(global-prettify-symbols-mode +1)

;; show trailing whitespace
;;(setq-default show-trailing-whitespace t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; remove the GUI Emacs clutter
(if window-system
    (tool-bar-mode -1)
)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; disable lockfiles
(setq create-lockfiles nil)

;; properly delete tabs using backspace
(setq backward-delete-char-untabify-method 'hungry)

;; turn on line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;; window resizing
(global-set-key (kbd "M-C-h") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-l") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-j") 'shrink-window)
(global-set-key (kbd "M-C-k") 'enlarge-window)

;; disable some features and settings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
