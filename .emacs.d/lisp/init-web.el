;;--------------------------------;
;; Web
;;--------------------------------;

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(add-hook 'rsjsx-mode-hook (lambda() (electric-indent-mode -1)))
(add-hook 'typescript-mode-hook 'web-mode)

(with-no-warnings (defvaralias 'js-indent-level 'tab-width))
(add-hook 'rjsx-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))
(add-hook 'web-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))
(add-hook 'js-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))
(add-hook 'js2-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))
(add-hook 'typescript-mode-hook (lambda () (defvaralias 'js-indent-level 'tab-width)))

(provide 'init-web)
