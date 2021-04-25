;;--------------------------------;
;; Common Lisp
;;--------------------------------;

(require 'init-lispy)

(add-hook 'lisp-mode-hook 'boogs/init-lispy)
(setq inferior-lisp-program "sbcl")

;; ;;--------------------------------

(when (require 'helm-sly nil 'noerror)
  (global-helm-sly-mode)
  (add-to-list 'helm-source-names-using-follow "Lisp xrefs"))

(provide 'init-lisp)
