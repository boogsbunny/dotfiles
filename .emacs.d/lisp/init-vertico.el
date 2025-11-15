;;--------------------------------------------------------------------
;; vertico
;;--------------------------------------------------------------------

(require 'vertico)
(require 'vertico-buffer)

(setq vertico-cycle t
      vertico-buffer-display-action '(display-buffer-reuse-window))

(define-key vertico-map (kbd "M-TAB") 'vertico-quick-insert)

(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)

(define-key vertico-map (kbd "M-n") 'vertico-next-group)
;; (define-key vertico-map (kbd "M-p") 'vertico-previous-group)
(define-key vertico-map (kbd "M-j") 'vertico-next-group)
(define-key vertico-map (kbd "M-k") 'vertico-previous-group)

(define-key vertico-map (kbd "C-<backspace>") 'vertico-directory-delete-word)
(define-key vertico-map (kbd "C-l") 'vertico-directory-delete-word)
(define-key vertico-map (kbd "TAB") 'vertico-directory-enter)

(vertico-mode)
;; (vertico-buffer-mode)

(provide 'init-vertico)
