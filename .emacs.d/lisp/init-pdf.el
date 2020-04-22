;;--------------------------------;
;; PDF tools
;;--------------------------------;

;; open pdfs scaled to fit page
(setq-default pdf-view-display-size 'fit-page)

;; more fine-grained zomming
(setq pdf-view-resize-factor 1.1)
(setq image-scaling-factor 1)

(setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12"))

;; midnight color scheme
(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(pdf-tools-install t t t)

(provide 'init-pdf)
