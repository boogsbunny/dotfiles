;;------------------------------------------------------------

(defconst user-init-dir
	(cond ((boundp 'user-emacs-directory)
				 user-emacs-directory)
				((boundp 'user-init-directory)
				 user-init-directory)
				(t "~/.emacs.d/")))

(defun load-user-file (file)
	(interactive "f")
	"Load a file in current user's configuration directory"
	(load-file (expand-file-name file user-init-dir)))

;;------------------------------------------------------------

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'zenburn t)

;;------------------------------------------------------------

(load-user-file "init/early.el")
(load-user-file "init/defaults.el")
(load-user-file "init/defuns.el")
(load-user-file "init/version-control.el")
(load-user-file "init/org-mode.el")
(load-user-file "init/evil.el")
(load-user-file "init/docker.el")
(load-user-file "init/paredit.el")
(load-user-file "init/rcirc.el")
(load-user-file "init/tmp-files.el")
(load-user-file "init/yasnippet.el")
(load-user-file "init/ansi-term.el")
(load-user-file "init/autocomplete.el")
(load-user-file "init/company.el")
(load-user-file "init/dired.el")
(load-user-file "init/flycheck.el")
(load-user-file "init/lisp.el")

;;------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (magit use-package evil))))
