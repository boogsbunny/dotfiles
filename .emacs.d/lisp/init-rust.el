;;--------------------------------;
;; Rust mode
;;--------------------------------;

;; Rust style guide recommends spaces for indentation
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(setq rust-format-on-save t)

(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(provide 'init-rust)
