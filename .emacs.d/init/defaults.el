;;--------------------------------;
;; Default settings
;;--------------------------------;

;; Encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Ignore bell
(setq ring-bell-function #'ignore)

;; Highlight column (vertical line) of text
;;(require 'column-marker)

;; Save copied text from outside emacs when yanking
(setq save-interprogram-paste-before-kill t)

;; make yes-or-no prompts y-or-n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Make cursor width of the character it is under
(setq x-stretch-cursor t)

;; set tabs to 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; disable cursor blinking
(blink-cursor-mode 0)

;; highlight current line
(global-hl-line-mode 1)

;; highlight matching parenthesis
(show-paren-mode 1)

;; show cursor position within line
(column-number-mode 1)

;; prettify symbols
(global-prettify-symbols-mode +1)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remove the GUI Emacs clutter
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

;; Turn on line numbers
(defvar linum-format)
(global-linum-mode t)
(setq linum-format
      (lambda (line)
	(propertize
	 (format
	  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
	    (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;; SLIME clear buffer
(global-set-key (kbd "C-l") 'slime-repl-clear-buffer)

;; ~a~% shortcut
(global-set-key (kbd "C-.") '"~a~%")

;; ` shortcut
(global-set-key (kbd "C-'") '"`")

;; delete current input
;;(global-set-key (kbd "C-d") 'slime-repl-delete-current-input)


;; disable some features and settings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
