;;------------------------------------------------------------
;; INIT
;;------------------------------------------------------------

;; additional config in 'lisp' folder
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;; site-lisp folder for local packages on MacOSX
;; (add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

;; separate user files with cache files
(setq user-emacs-directory "~/.cache/emacs/")

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

(require 'init-packages)
(require 'init-defuns)
(require 'init-defaults)
(require 'init-styles)

;;------------------------------------------------------------
;; CONFIGS
;;------------------------------------------------------------

(require 'functions)

(with-eval-after-load 'dired (require 'init-dired))
(with-eval-after-load 'eshell (require 'init-eshell))
(with-eval-after-load 'elfeed (require 'init-elfeed))
(with-eval-after-load 'erc (require 'init-erc))
(with-eval-after-load 'tramp (require 'init-tramp))

(setq evil-want-keybinding nil
      evil-want-integration t)
(when (require 'evil nil t) (require 'init-evil))

(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode))

(when (require 'rainbow-mode nil t)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook js-mode-hook js2-mode-hook typescript-mode python-mode))
    (add-hook hook 'rainbow-mode)))

(require 'init-web)

(when (require 'company nil t)
  (setq company-idle-delay nil))

(setq org-roam-v2-ack t)
(with-eval-after-load 'org (require 'init-org))
(autoload 'helm-org-switch "org")

(with-eval-after-load 'exwm
  (require 'init-exwm))

(with-eval-after-load 'magit (require 'init-magit))
(with-eval-after-load 'eww (require 'init-eww))

(when (require 'helm-config nil t) (require 'init-helm))

(with-eval-after-load 'notmuch (require 'init-notmuch))
(autoload 'helm-notmuch-switch "notmuch")

(when (require 'helm-selector nil :noerror)
  (global-set-key (kbd "C-h i") 'helm-selector-info))

(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "M-s f") 'helm-restclient)
  (add-to-list 'helm-source-names-using-follow "Sources")
  (with-eval-after-load 'company-restclient
    (add-to-list 'company-backends 'company-restclient)
    (add-hook 'restclient-mode-hook 'company-mode)
    (define-key restclient-mode-map (kbd "M-<tab>") (if (require 'helm-company nil t)
                                                        'helm-company
                                                      'company-complete))))
;;; System packages
(global-set-key (kbd "C-x c #") 'helm-system-packages)

(with-eval-after-load 'lisp-mode (require 'init-lisp))
(add-hook 'emacs-lisp-mode-hook 'boogs/init-lispy)

(with-eval-after-load 'eglot (require 'init-eglot))

(with-eval-after-load 'rust-mode (require 'init-rust))
(defvaralias 'rust-indent-offset 'tab-width)

(with-eval-after-load 'python-mode (require 'init-python))
(with-eval-after-load 'go-mode (require 'init-go))

(with-eval-after-load 'sql (require 'init-sql))

(with-eval-after-load 'ledger-mode (require 'init-ledger))

(with-eval-after-load 'emms (require 'init-emms))

(with-eval-after-load 'slack (require 'init-slack))

(with-eval-after-load 'transmission
  ;; `transmission' will fail to start and will not run any hook if the daemon
  ;; is not up yet.
  ;; We need to advice the function :before to guarantee it starts.
  (defun boogs/transmission-start-daemon ()
    (unless (member "transmission-da"
                    (mapcar
                     (lambda (pid) (alist-get 'comm (process-attributes pid)))
                     (list-system-processes)))
      (call-process "transmission-daemon")
      (sleep-for 1)))
  (advice-add 'transmission :before 'boogs/transmission-start-daemon)
  (setq transmission-refresh-modes '(transmission-mode transmission-files-mode transmission-info-mode transmission-peers-mode)
        transmission-refresh-interval 1))


;;------------------------------------------------------------
;; CLEANUP
;;------------------------------------------------------------

;; remove `customize' clutter
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
(put 'scroll-left 'disabled nil)
