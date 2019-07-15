(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "~/.emacs.d/mirror-elpa/melpa/")
                         ("org"   . "~/.emacs.d/mirror-elpa/org/")
                         ("gnu"   . "~/.emacs.d/mirror-elpa/gnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; TODO states
(setq org-todo-keywords
			'((sequence
				 "TODO(t)" ; doing later
				 "NEXT(n)" ; doing now or soon
				 "|"
				 "DONE(d)" ; finished
				 "CANCELED(c)" ; canceled
				 )))

;; Encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Ignore bell
(setq ring-bell-function 'ignore)

;; Backward kill line
(bind-key "C-<backspace>" (lambda ()
                            (interactive)
                            (kill-line 0)
                            (indent-according-to-mode)))

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; Make cursor width of the character it is under
(setq x-stretch-cursor t)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; prettify symbols
(global-prettify-symbols-mode +1)

;; Save copied text from outside emacs when yanking
(setq save-interprogram-paste-before-kill t)

;; save backups in one directory aka within Emacs directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; make yes-or-no prompts y-or-n prompts
(fset 'yes-or-no-p 'y-or-n-p)


;; disable some features and settings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Turn on line numbers
(defvar linum-format)
(global-linum-mode t)
(setq linum-format
      (lambda (line)
	(propertize
	 (format
	  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
	    (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't show trailing whitespace in calendar mode
(add-hook 'calendar-mode-hook
          (function (lambda () (setq show-trailing-whitespace nil))))

;; Remove the GUI Emacs clutter
(if window-system
    (tool-bar-mode -1)
)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;tree plugin like NerdTree for Vim
(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
;(global-set-key (kbd "C-n") 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;utility package to collect various icon fonts
;(add-to-list 'load-path "C:/.emacs.d/all-the-icons")
;(require 'all-the-icons)

;; disable lockfiles
(setq create-lockfiles nil)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'all-the-icons)

(load-theme 'zenburn t)

;(setq explicit-shell-file-name "C:/APPS/cygwin64/bin/bash.exe")
;(setq shell-file-name explicit-shell-file-name)
;(add-to-list 'exec-path "C:/APPS/cygwin64/bin")

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;(progn
  ;; make indentation commands use space only (never tab character)
;  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1 to 26, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting
  ;; mixed space and tab
;)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 2) ; emacs 23.1 to 26 default to 8

;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(helm-dash key-chord circadian company tide js2-mode smart-tabs-mode all-the-icons magit typescript-mode apache-mode yasnippet angular-snippets angular-mode use-package ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; show cursor position within line
(column-number-mode 1)

;; properly delete tabs using backspace
(setq backward-delete-char-untabify-method 'hungry)

;; indent with tabs and align with spaces in various languages
(smart-tabs-insinuate 'c 'javascript 'python)

;; smart tabs for js2-mode
(smart-tabs-advice js2-indent-line js2-basic-offset)


;; key-chord to map 'jk' to Escape
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

 (eval-after-load 'evil-maps
 	'(progn
;; 		 (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
 		 (define-key evil-motion-state-map (kbd ";") 'evil-ex)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my-other-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
          (progn
            (delete-char trailnewlines)))))))

;; Vim like shifting selected text to left "<<" or right ">>"
(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))


(defun move-file (new-location)
	"Write this file to NEW-LOCATION, and delete the old one."
	(interactive (list (expand-file-name
											(if buffer-file-name
													(read-file-name "Move file to: ")
												(read-file-name "Move file to: "
																				default-directory
																				(expand-file-name (file-name-nondirectory (buffer-name))
																													default-directory))))))
	(when (file-exists-p new-location)
		(delete-file new-location))
	(let ((old-location (expand-file-name (buffer-file-name))))
		(message "old file is %s and new file is %s"
						 old-location
						 new-location)
		(write-file new-location t)
		(when (and old-location
							 (file-exists-p new-location)
							 (not (string-equal old-location new-location)))
			(delete-file old-location))))

;;(bind-key "C-x C-m" #'move-file)

;; hydra
(use-package hydra
  :config
  (setq hydra-lv nil))

;; Zooming
(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("_" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

(bind-keys ("C-x C-0" . hydra-zoom/body)
           ("C-x C-=" . hydra-zoom/body)
           ("C-x C--" . hydra-zoom/body)
           ("C-x C-+" . hydra-zoom/body))
