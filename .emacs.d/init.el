;;------------------------------------------------------------
;; INIT
;;------------------------------------------------------------

;; additional config in 'lisp' folder
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;; site-lisp folder for local packages on MacOSX
;; (add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

;; separate user files with cache files
(setq user-emacs-directory "~/.cache/emacs/")

(require 'init-packages)
(require 'init-defuns)
(require 'init-defaults)
(require 'init-styles)

;; site-lisp folder for local packages on GNU/Linux
(defun boogs/package-refresh-load-path (path)
  "Add every non-hidden sub-folder of PATH to `load-path'."
  (when (file-directory-p path)
    (dolist (dir (directory-files path t "^[^\\.]"))
      (when (file-directory-p dir)
        (setq load-path (add-to-list 'load-path dir))
        (dolist (subdir (directory-files dir t "^[^\\.]"))
          (when (file-directory-p subdir)
            (setq load-path (add-to-list 'load-path subdir))))))))

(let ((site-lisp (expand-file-name "site-lisp/" "~/.local/share/emacs/")))
  (add-to-list 'load-path site-lisp)
  (boogs/package-refresh-load-path site-lisp))

;;------------------------------------------------------------
;; CONFIGS
;;------------------------------------------------------------

(with-eval-after-load 'dired (require 'init-dired))

(setq evil-want-keybinding nil
      evil-want-integration t)
(when (require 'evil nil t) (require 'init-evil))

(when (require 'company nil t)
  (setq company-idle-delay nil))

(with-eval-after-load 'org (require 'init-org))
(autoload 'helm-org-switch "org")

(with-eval-after-load 'magit (require 'init-magit))

(when (require 'helm-config nil t) (require 'init-helm))

(with-eval-after-load 'notmuch (require 'init-notmuch))
(autoload 'helm-notmuch-switch "notmuch")

(when (require 'pdf-tools nil t) (require 'init-pdf))

(with-eval-after-load 'lisp-mode (require 'init-lisp))
(add-hook 'emacs-lisp-mode-hook 'boogs/init-lispy)

(with-eval-after-load 'lsp (require 'init-lsp))

(with-eval-after-load 'rust-mode (require 'init-rust))
(with-eval-after-load 'elpy-mode (require 'init-python))
(with-eval-after-load 'csharp-mode (require 'init-csharp))

(with-eval-after-load 'docker (require 'init-docker))
(with-eval-after-load 'ledger-mode (require 'init-ledger))

(when (require 'slack nil t) (require 'init-slack))
(require 'init-jira)

;;------------------------------------------------------------
;; CLEANUP
;;------------------------------------------------------------

;; remove `customize' clutter
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
