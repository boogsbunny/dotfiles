;;--------------------------------------------------------------------
;; embark
;;--------------------------------------------------------------------
(require 'embark)

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(setq prefix-help-command #'embark-prefix-help-command)

(defun boogs/embark-magit-status (file)
  "Run magit-status on the directory containing FILE."
  (interactive "f")
  (magit-status (if (file-directory-p file)
                    file
                  (file-name-directory file))))

(define-key embark-file-map (kbd "m") #'boogs/embark-magit-status)
(define-key embark-file-map (kbd "M") #'boogs/embark-magit-status)
(define-key embark-file-map (kbd "f") #'consult-ls-git)

(setq embark-indicators
      '(embark-mixed-indicator ; default is embark-mixed-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(provide 'init-embark)
