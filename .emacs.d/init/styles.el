;;--------------------------------;
;; Styles
;;--------------------------------;

(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-dark t)

(use-package powerline
  :ensure t)
(powerline-default-theme)

(use-package airline-themes
  :ensure t)
(load-theme 'airline-doom-one t)
(setq powerline-height 30)
