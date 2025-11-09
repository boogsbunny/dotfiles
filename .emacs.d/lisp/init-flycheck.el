;;--------------------------------------------------------------------
;; flycheck
;;--------------------------------------------------------------------

(defun boogs/flycheck-and-whitespace-mode ()
  ""
  (interactive)
  (if (derived-mode-p 'text-mode)
      (flyspell-mode)
    (flycheck-mode 'toggle)
    (if flyspell-mode (flyspell-mode 0) (flyspell-prog-mode)))
  (whitespace-mode 'toggle))

(global-set-key (kbd "<f-9>") 'boogs/flycheck-and-whitespace-mode)

(provide 'init-flycheck)
