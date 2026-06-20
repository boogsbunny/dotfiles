;;--------------------------------------------------------------------
;; eglot
;;--------------------------------------------------------------------

(require 'patch-eglot)
(eglot-booster-mode)

(fset #'jsonrpc--log-event #'ignore)

(setq eglot-autoreconnect t
      eglot-autoshutdown t
      eglot-confirm-server-edits nil
      eglot-connect-timeout nil
      eglot-events-buffer-size 0
      eglot-extend-to-xref t
      eglot-ignored-server-capabilities '(:hoverProvider
                                          :documentHighlightProvider)
      eglot-report-progress t
      eglot-sync-connect nil)

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            ;; Show flymake diagnostics first.
            (setq eldoc-documentation-functions
                  (cons #'flymake-eldoc-function
                        (remove #'flymake-eldoc-function
                                eldoc-documentation-functions)))
            ;; Make sure Eldoc will show us all of the feedback at
            ;; point.
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

(global-set-key (kbd "C-c e e") 'eglot)
(global-set-key (kbd "C-c e s") 'eglot-shutdown)
(global-set-key (kbd "C-c e a") 'eglot-shutdown-all)

(provide 'init-eglot)
