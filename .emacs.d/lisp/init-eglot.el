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

;; auto-completion
(require 'company)
(add-hook 'go-mode-hook #'company-mode)
(require 'yasnippet)
(add-hook 'go-mode-hook #'yas-minor-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.1
      company-show-numbers nil
      company-require-match nil
      company-dabbrev-ignore-case t
      company-dabbrev-code-ignore-case t
      completion-ignore-case t)

(provide 'init-eglot)
