;;--------------------------------------------------------------------
;; prescient
;;--------------------------------------------------------------------

(require 'prescient)
(require 'corfu-prescient)
(require 'vertico-prescient)

(setq completion-preview-sort-function #'prescient-completion-sort)

(corfu-prescient-mode)
(vertico-prescient-mode)

(provide 'init-prescient)
