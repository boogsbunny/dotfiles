;;--------------------------------;
;; C#
;;--------------------------------;

(defun boogs/csharp-mode ()
  (company-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (eletric-pair-local-mode 1))

(add-hook 'csharp-mode-hook 'boogs/csharp-mode)

(require 'init-lsp)

(provide 'init-csharp)
