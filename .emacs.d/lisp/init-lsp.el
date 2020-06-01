;;------------------------------------;
;; LSP mode (Language Server Protocol)
;;------------------------------------;

(setq lsp-keymap-prefix "C-c l")

(require 'lsp-mode)
(require 'lsp-ui)
(require 'helm-lsp)
(require 'company-lsp)

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode)

(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'csharp-mode-hook #'lsp-deferred)
(add-hook 'js2-mode-hook #'lsp-deferred)
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'elpy-mode-hook #'lsp-deferred)

(setq lsp-prefer-capf t
			lsp-idle-delay 0.500)

(provide 'init-lsp)
