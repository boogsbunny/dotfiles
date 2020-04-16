;;------------------------------------;
;; LSP mode (Language Server Protocol)
;;------------------------------------;

;; (use-package lsp-mode
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   :ensure t
;;   :init (setq lsp-keymap-prefix "s-l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (rust-mode . lsp)
;;          ;; if you want which-key integration
;;          ;;(lsp-mode . lsp-enable-which-key-integration)
;;          )
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;; (push 'company-lsp company-backends)

;; (use-package helm-lsp
;;   :ensure t
;;   :commands helm-lsp-workspace-symbol)

(provide 'init-lsp)
