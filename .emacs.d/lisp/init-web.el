;;--------------------------------------------------------------------
;; web
;;--------------------------------------------------------------------

(setq treesit-extra-load-path '("~/.guix-extra-profiles/default/default/lib/tree-sitter"))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))

(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook (lambda () (electric-indent-mode -1)))
  (add-hook 'tsx-ts-mode-hook (lambda () (prettier-mode 1)))
  (define-key typescript-ts-mode-map (kbd "<return>") 'newline-and-indent))

(with-eval-after-load 'tsx-ts-mode
  (add-hook 'tsx-ts-mode-hook (lambda () (electric-indent-mode -1)))
  (add-hook 'tsx-ts-mode-hook (lambda () (prettier-mode 1)))
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure))

(with-eval-after-load 'javascript-ts-mode
  (add-hook 'javascript-ts-mode-hook (lambda () (electric-indent-mode -1)))
  (add-hook 'javascript-ts-mode-hook 'eglot-ensure))

(with-eval-after-load 'rjsx-mode
  (add-hook 'rjsx-mode-hook (lambda () (electric-indent-mode -1)))
  (add-hook 'rjsx-mode-hook 'eglot-ensure))

(with-eval-after-load 'html-ts-mode
  (defun boogs/html-ts-mode-hook ()
    "Hooks for HTML Treesitter mode."
    (setq indent-tabs-mode nil
          tab-width 2))
  (add-hook 'html-ts-mode-hook 'boogs/html-ts-mode-hook)
  (add-hook 'html-ts-mode-hook 'add-node-modules-path)
  (add-hook 'html-ts-mode-hook 'prettier-js-mode-on-save)
  (add-hook 'html-ts-mode-hook 'eglot-ensure))

(with-eval-after-load 'css-ts-mode
  (defun boogs/css-ts-mode-hook ()
    "Hooks for CSS Treesitter mode."
    (setq indent-tabs-mode nil
          tab-width 2))
  (add-hook 'css-ts-mode-hook 'boogs/css-ts-mode-hook))

(require 'init-lass)
(add-to-list 'auto-mode-alist '("\\.lass\\'" . lass-mode))
(autoload 'lass-mode "lass" "Lass Support" t)

(provide 'init-web)
