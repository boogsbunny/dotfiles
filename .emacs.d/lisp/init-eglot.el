;;--------------------------------------------------------------------
;; eglot
;;--------------------------------------------------------------------

(require 'patch-eglot)
(eglot-booster-mode)

(fset #'jsonrpc--log-event #'ignore)

(setq eglot-events-buffer-size 0
      eglot-sync-connect nil
      eglot-connect-timeout nil
      eglot-ignored-server-capabilities '(:hoverProvider
                                          :documentHighlightProvider)
      eglot-autoshutdown t)

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            "Make sure Eldoc will show us all of the feedback at point."
            (setq-local eldoc-documentation-strategy
                        #'eldoc-documentation-compose)))

(defvar eglot-format-keys-map (make-sparse-keymap)
  "Keymap for Eglot formatting and Flymake navigation commands.")

(define-key eglot-mode-map (kbd "C-c e f") eglot-format-keys-map)

(define-key eglot-format-keys-map (kbd "n") #'flymake-goto-next-error)
(define-key eglot-format-keys-map (kbd "p") #'flymake-goto-prev-error)
(define-key eglot-format-keys-map (kbd "f") #'eglot-format)
(define-key eglot-format-keys-map (kbd "F") #'eglot-format-buffer)

(define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c e d") #'eglot-find-typeDefinition)
(define-key eglot-mode-map (kbd "C-c e D") #'eglot-find-declaration)
(define-key eglot-mode-map (kbd "C-c e R") #'eglot-reconnect)

(provide 'init-eglot)
