;;--------------------------------------------------------------------
;; embark
;;--------------------------------------------------------------------
(require 'embark)

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(setq prefix-help-command #'embark-prefix-help-command)

(define-key embark-buffer-map (kbd "M") 'magit-status)
(define-key embark-file-map (kbd "M") 'magit-status)

(define-key embark-buffer-map (kbd "m") 'magit-status)
(define-key embark-file-map (kbd "m") 'magit-status)

(setq embark-indicators
      '(embark-mixed-indicator ; default is embark-mixed-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(require 'embark-consult)
(add-hook 'embark-collect-mode 'consult-preview-at-point-mode)

(provide 'init-embark)
