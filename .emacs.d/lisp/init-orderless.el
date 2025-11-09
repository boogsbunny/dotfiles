;;--------------------------------------------------------------------
;; orderless
;;--------------------------------------------------------------------

(require 'orderless)

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles partial-completion)))
      completion-category-defaults nil
      completion-pcm-leading-wildcard t)

(provide 'init-orderless)
