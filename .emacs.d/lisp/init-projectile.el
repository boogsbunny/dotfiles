;;--------------------------------------------------------------------
;; projectile
;;--------------------------------------------------------------------

(require 'projectile)

(add-to-list 'projectile-globally-ignored-directories "*node_modules")

(projectile-mode)

(provide 'init-projectile)
