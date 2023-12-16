;;--------------------------------------------------------------------
;; rust mode
;;--------------------------------------------------------------------

(setq rust-format-on-save t)

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook #'tree-sitter-mode)
(add-hook 'rust-mode-hook #'tree-sitter-hl-mode)

;; eglot
(add-hook 'rust-mode-hook 'eglot-ensure)

(provide 'init-rust)
