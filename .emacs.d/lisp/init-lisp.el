;;--------------------------------;
;; Common Lisp
;;--------------------------------;

(require 'init-lispy)

(add-hook 'lisp-mode-hook 'boogs/init-lispy)
(setq inferior-lisp-program "$HOME/.guix-profile/lib/sbcl")

(whitespace-mode 1)

(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

;; ;;--------------------------------

(when (require 'helm-sly nil 'noerror)
  (global-helm-sly-mode)
  (add-to-list 'helm-source-names-using-follow "Lisp xrefs"))

(with-eval-after-load 'sly
  (require 'init-sly))

(provide 'init-lisp)
