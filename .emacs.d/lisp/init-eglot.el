;;--------------------------------------------------------------------
;; eglot
;;--------------------------------------------------------------------

;; (add-to-list 'eglot-server-programs
;;              '(web-mode "/home/boogs/.npm-packages/bin/typescript-language-server --stdio"))

(add-to-list 'eglot-server-programs
             '(web-mode "typescript-language-server" "--stdio"))

(define-key eglot-mode-map (kbd "C-c e f n") #'flymake-goto-next-error)
(define-key eglot-mode-map (kbd "C-c e f p") #'flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)

(provide 'init-eglot)
