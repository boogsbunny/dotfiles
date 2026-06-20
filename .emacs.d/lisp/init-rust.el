;;--------------------------------------------------------------------
;; rust
;;--------------------------------------------------------------------

(setq rust-format-on-save t)

(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(with-eval-after-load 'flycheck-rust
  (setq flycheck-rust-cargo-check-args
        '("--all-targets" "--all-features")))

(defun boogs/rust-cargo-root ()
  "Return the nearest Cargo workspace/package root."
  (or (locate-dominating-file default-directory "Cargo.lock")
      (locate-dominating-file default-directory "Cargo.toml")))

(defun boogs/rust-analyzer-contact (_interactive)
  "Start rust-analyzer for the nearest Cargo workspace."
  (let* ((root (boogs/rust-cargo-root))
         (manifest (and root (expand-file-name "Cargo.toml" root))))
    `("rust-analyzer"
      :initializationOptions
      (:linkedProjects [,manifest]
       :cargo (:targetDir "target/rust-analyzer")
       :procMacro (:enable t)
       :check (:workspace t)))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . boogs/rust-analyzer-contact)))

(provide 'init-rust)
