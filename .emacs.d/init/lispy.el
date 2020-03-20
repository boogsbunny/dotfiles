;;--------------------------------;
;; Lispy
;;--------------------------------;

(use-package lispy
  :ensure t)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(use-package rainbow-delimiters
  :ensure t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
