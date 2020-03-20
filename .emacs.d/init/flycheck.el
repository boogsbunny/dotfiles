(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (bind-key "M-n" 'flycheck-next-error flycheck-mode-map)
  (bind-key "M-p" 'flycheck-previous-error flycheck-mode-map))
