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

(load-user-file "init/packages.el")
(load-user-file "init/defaults.el")
(load-user-file "init/defuns.el")
(load-user-file "init/version-control.el")
(load-user-file "init/org-mode.el")
(load-user-file "init/evil.el")
(load-user-file "init/docker.el")
(load-user-file "init/rcirc.el")
(load-user-file "init/tmp-files.el")
(load-user-file "init/yasnippet.el")
(load-user-file "init/autocomplete.el")
(load-user-file "init/company.el")
(load-user-file "init/dired.el")
(load-user-file "init/flycheck.el")
(load-user-file "init/lisp.el")
(load-user-file "init/helm.el")
(load-user-file "init/rust.el")
(load-user-file "init/styles.el")
(load-user-file "init/ledger.el")
(load-user-file "init/dashboard.el")
(load-user-file "init/python.el")
(load-user-file "init/projectile.el")
(load-user-file "init/restclient.el")
(load-user-file "init/lsp.el")
(load-user-file "init/lispy.el")
(load-user-file "init/pdf.el")
;; (load-user-file "init/mu4e.el")
(load-user-file "init/notmuch.el")

;;------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d6db7498e2615025c419364764d5e9b09438dfe25b044b44e1f336501acd4f5b" default)))
 '(package-selected-packages
   (quote
    (mu4e pdf-tools cargo dap-mode powerline helm slime evil restclient elpy dashboard evil-magit evil-collection ledger-mode zenburn-theme yasnippet use-package solarized-theme slime-docker rust-mode paredit org-journal org-bullets magit leetcode key-chord helm-projectile flycheck evil-org dockerfile-mode docker company auto-complete airline-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
