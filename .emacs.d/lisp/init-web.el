;;--------------------------------------------------------------------;
;; web
;;--------------------------------------------------------------------;

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(add-hook 'rsjsx-mode-hook (lambda () (electric-indent-mode -1)))
(add-hook 'typescript-mode-hook 'web-mode)

(defun boogs/web-mode-hook ()
  "Hooks for Web mode."
  (setq indent-tabs-mode nil
        tab-width 2
        web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2))

(add-hook 'web-mode-hook  'boogs/web-mode-hook)
(add-hook 'web-mode-hook 'prettier-mode)

(require 'init-lass)
(add-to-list 'auto-mode-alist '("\\.lass\\'" . lass-mode))
(autoload 'lass-mode "lass" "Lass Support" t)

(provide 'init-web)
