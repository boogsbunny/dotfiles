;;--------------------------------------------------------------------
;; pdf tools
;;--------------------------------------------------------------------

;; open pdfs scaled to fit page
(setq-default pdf-view-display-size 'fit-page)

;; more fine-grained zomming
(setq pdf-view-resize-factor 1.1)
(setq image-scaling-factor 1)

;; orange / black
;; (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12"))
(add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
(add-hook 'pdf-view-mode-hook (lambda () (global-display-line-numbers-mode 0)))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))

(pdf-tools-install t t t)

(require 'image-roll)

(provide 'init-pdf)
