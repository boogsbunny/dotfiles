;;--------------------------------------------------------------------
;; init
;;--------------------------------------------------------------------

(require 'init-modeline)
(require 'init-perspective)
(require 'init-defaults)
(require 'init-olivetti)
(require 'init-styles)
(require 'init-defuns)
(require 'init-projectile)
(require 'init-projectile-perspective)
(require 'init-marginalia)
(require 'init-icons)
(require 'init-orderless)
(require 'init-vertico)
(require 'init-embark)
(require 'init-consult)
(require 'init-consult-notmuch)
(require 'init-corfu)
(require 'init-prescient)
(require 'init-avy)
(require 'init-web)
(require 'functions)

(with-eval-after-load 'dired (require 'init-dired))
(with-eval-after-load 'eshell (require 'init-eshell))
(with-eval-after-load 'shell (require 'init-shell))
(with-eval-after-load 'elfeed (require 'init-elfeed))
(with-eval-after-load 'erc (require 'init-erc))
(with-eval-after-load 'tramp (require 'init-tramp))

(when (require 'evil nil t) (require 'init-evil))
(with-eval-after-load 'magit (require 'init-magit))

(when (display-graphic-p)
	(require 'init-pdf))

(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode))

(when (require 'rainbow-mode nil t)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook js-mode-hook js2-mode-hook typescript-mode python-mode))
    (add-hook hook 'rainbow-mode)))

(setq org-roam-v2-ack t)
(with-eval-after-load 'org (require 'init-org))

(with-eval-after-load 'exwm
  (require 'init-exwm))

(with-eval-after-load 'eww (require 'init-eww))

(when (executable-find "guix")
  (require 'init-guix))

(when (executable-find "notmuch")
	(with-eval-after-load 'notmuch (require 'init-notmuch)))

(with-eval-after-load 'lisp-mode (require 'init-lisp))
(with-eval-after-load 'scheme (require 'init-scheme))
(add-hook 'emacs-lisp-mode-hook 'boogs/init-lispy)

(with-eval-after-load 'eglot (require 'init-eglot))

(with-eval-after-load 'rust-mode (require 'init-rust))
(defvaralias 'rust-indent-offset 'tab-width)

(with-eval-after-load 'zig-mode (require 'init-zig))
(with-eval-after-load 'python-mode (require 'init-python))
(with-eval-after-load 'go-mode (require 'init-go))

(with-eval-after-load 'sql (require 'init-sql))

(with-eval-after-load 'ledger-mode (require 'init-ledger))

(with-eval-after-load 'emms (require 'init-emms))

(with-eval-after-load 'slack (require 'init-slack))

(with-eval-after-load 'transmission
  ; `transmission' will fail to start and will not run any hook if the daemon
  ; is not up yet.
  ; We need to advice the function :before to guarantee it starts.
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


;;--------------------------------------------------------------------
;; cleanup
;;--------------------------------------------------------------------

;; remove `customize' clutter
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
(put 'scroll-left 'disabled nil)
