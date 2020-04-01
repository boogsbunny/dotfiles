;;--------------------------------;
;; Projectile
;;--------------------------------;

(use-package projectile
  :ensure t)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-project-search-path '("~/Documents/workspace/boogs" "~/Repos"))

(use-package ripgrep
  :ensure t)
