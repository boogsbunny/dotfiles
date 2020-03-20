;;--------------------------------;
;; Rust mode
;;--------------------------------;

(use-package rust-mode
  :ensure t)

;; Rust style guide recommends spaces for indentation
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(setq rust-format-on-save t)

(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(use-package cargo
  :ensure t)

(add-hook 'rust-mode-hook 'cargo-minor-mode)
