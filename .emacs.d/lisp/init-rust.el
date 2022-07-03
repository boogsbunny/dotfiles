;;--------------------------------;
;; Rust mode
;;--------------------------------;

(setq rust-format-on-save t)

(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(require 'init-lsp)

(provide 'init-rust)
