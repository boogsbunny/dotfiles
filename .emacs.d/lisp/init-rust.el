;;--------------------------------------------------------------------
;; rust
;;--------------------------------------------------------------------

(setq rust-format-on-save t)

(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

(add-hook 'rust-mode-hook 'cargo-minor-mode)


(provide 'init-rust)
