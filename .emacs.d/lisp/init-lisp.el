;;--------------------------------;
;; Common Lisp
;;--------------------------------;

(require 'init-lispy)

(add-hook 'lisp-mode-hook 'boogs/init-lispy)
(setq inferior-lisp-program "$HOME/.guix-profile/lib/sbcl")

(add-hook 'lisp-mode-hook
          (function (lambda () (setq fill-column (string-to-number "100")))))

(whitespace-mode 1)

(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(when (require 'helm-sly nil 'noerror)
  (global-helm-sly-mode)
  (add-to-list 'helm-source-names-using-follow "Lisp xrefs"))

(with-eval-after-load 'sly
  (require 'init-sly))

(require 'company)
(add-hook 'lisp-mode-hook #'company-mode)
(define-key lisp-mode-map (kbd "<tab>") 'sly-mrepl-indent-and-complete-symbol)

(provide 'init-lisp)
