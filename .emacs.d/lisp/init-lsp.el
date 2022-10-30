;;------------------------------------;
;; LSP mode (Language Server Protocol)
;;------------------------------------;

(setq lsp-keymap-prefix "C-c l")

(require 'lsp-mode)
(require 'lsp-ui)
(require 'helm-lsp)
;; (require 'company-lsp)

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode)

(setq lsp-completion-provider :capf
      lsp-completion-show-detail t
      lsp-completion-show-kind t
      lsp-headerline-breadcrumb-enable t
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-hover t
      lsp-ui-sideline-ignore-duplicate t
      lsp-ui-sideline-show-code-actions t
      lsp-ui-sideline-show-symbol t

      lsp-ui-doc-enable t
      lsp-ui-doc-header t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-max-height 30
      lsp-ui-doc-max-width 120
      lsp-ui-doc-position 'at-point
      lsp-ui-doc-use-childframe t
      lsp-ui-doc-use-webkit nil
      lsp-ui-doc-show-with-cursor t

      lsp-ui-flycheck-enable t
      lsp-ui-imenu-enable t
      lsp-ui-imenu-kind-position 'top
      lsp-ui-peek-enable t
      lsp-ui-peek-fontify 'on-demand
      lsp-ui-peek-list-width 50
      lsp-ui-peek-peek-height 20

      lsp-print-io t
      lsp-trace t
      lsp-print-performance t
      lsp-auto-guess-root t
      lsp-document-sync-method 'incremental)

(add-hook 'html-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'js2-mode-hook #'lsp-deferred)
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)


(setq lsp-prefer-capf t
      lsp-idle-delay 0.500)

(provide 'init-lsp)
