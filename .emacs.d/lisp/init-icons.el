;;--------------------------------------------------------------------
;; all-the-icons
;;--------------------------------------------------------------------

(when (display-graphic-p)
  (require 'all-the-icons))

(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(provide 'init-icons)
