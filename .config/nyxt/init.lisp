(in-package #:nyxt-user)

;; import files
(nyxt::load-lisp "~/.config/nyxt/statusline.lisp")
(nyxt::load-lisp "~/.config/nyxt/stylesheet.lisp")

(load-after-system :nx-search-engines (nyxt-init-file "search-engines.lisp"))
(load-after-system :demeter)

(define-configuration (buffer web-buffer nosave-buffer)
    ((default-modes (append '(vi-normal-mode) %slot-default%))))

(define-configuration prompt-buffer
    ((default-modes (append '(vi-insert-mode) %slot-default%))))
