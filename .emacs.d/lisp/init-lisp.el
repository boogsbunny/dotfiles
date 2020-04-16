;;--------------------------------;
;; Common Lisp
;;--------------------------------;

(require 'init-lispy)

(add-hook 'lisp-mode-hook 'boogs/init-lispy)

;; slime setup
;; (when (file-exists-p (expand-file-name "~/quicklisp.lisp"))
;;   (use-package slime
;;     :ensure t
;;     :bind
;;     (("C-c C-v C-v" . slime-vari-describe-symbol))
;;     :config
;;     (setq slime-lisp-implementations
;; 	  '((sbcl  ("/usr/bin/sbcl") :coding-system utf-8-unix)
;;             (sbcl "--dynamic-space-size" "2GB")
;;             (sbcl  ("sbcl") :coding-system utf-8-unix)
;;             )
;; 	  slime-net-coding-system 'utf-8-unix
;;           slime-inhibit-pipelining nil
;; 	  slime-contribs '(slime-fancy slime-repl slime-scratch slime-trace-dialog))
;;     (add-hook 'slime-load-hook            (lambda () (require 'slime-fancy)))
;;     (add-hook 'lisp-mode-hook             'my-lisp-mode-hook)
;;     (add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-hook)
;;     (add-hook 'slime-repl-mode-hook       'my-lisp-repl-hook))
;;   )

;; ;; Set your lisp system and, optionally, some contribs
;; ;;(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
;; ;;(setq slime-contribs '(slime-fancy))

;; (defun my-lisp-mode-hook ()
;;   (paredit-mode +1)
;;   (setq indent-tabs-mode nil)
;;   (whitespace-mode 1))

;; (defun my-lisp-repl-hook ()
;;   (paredit-mode +1)
;;   '(lambda () (local-set-key) (kbd "C-l") 'slime-repl-clear-buffer))

;; ;;--------------------------------
;; ;; Helpers

;; (defun slime-inspect-macroexpand-1 ()
;;   (interactive)
;;   (slime-eval-async
;;       `(swank::to-string
;;         (swank:inspect-in-emacs
;;          (cl:macroexpand
;;           (swank::from-string
;;            ',(slime-sexp-at-point-or-error)))))))

;; (defun slime-inspect-macroexpand-all ()
;;   (interactive)
;;   (slime-eval-async
;;       `(swank::to-string
;;         (swank:inspect-in-emacs
;;          (swank-backend:macroexpand-all
;;           (swank::from-string
;;            ',(slime-sexp-at-point-or-error)))))))

(when (require 'helm-sly nil 'noerror)
  (global-helm-sly-mode)
  (add-to-list 'helm-source-names-using-follow "Lisp xrefs"))

(when (require 'helm-slime nil 'noerror)
  (global-helm-slime-mode)
  (add-to-list 'helm-source-names-using-follow "SLIME xrefs"))


(provide 'init-lisp)
