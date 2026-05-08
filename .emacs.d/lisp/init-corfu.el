;;--------------------------------------------------------------------
;; corfu
;;--------------------------------------------------------------------

(require 'corfu)

(define-key corfu-map (kbd "C-j") 'corfu-next)
(define-key corfu-map (kbd "C-k") 'corfu-previous)

(define-key corfu-map (kbd "C-n") 'corfu-next)
(define-key corfu-map (kbd "C-p") 'corfu-previous)

(define-key corfu-map (kbd "M-n") 'corfu-next)
(define-key corfu-map (kbd "M-p") 'corfu-previous)

(setq corfu-cycle t
      corfu-preview-current 'insert
      corfu-auto-delay 0.4)

(setq-default corfu-auto t)

(keymap-unset corfu-map "RET")

(global-corfu-mode)

(require 'kind-icon)

(setq kind-icon-use-icons t
      kind-icon-default-face 'corfu-default)

(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(provide 'init-corfu)
