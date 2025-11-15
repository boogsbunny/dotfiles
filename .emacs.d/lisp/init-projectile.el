;;--------------------------------------------------------------------
;; projectile
;;--------------------------------------------------------------------

(require 'projectile)

(dolist (dir '(".git" "node_modules" "dist" "build" "target" ".cache"))
	(add-to-list 'projectile-globally-ignored-directories dir))

(setq projectile-enable-caching t)

(projectile-mode)

(provide 'init-projectile)
