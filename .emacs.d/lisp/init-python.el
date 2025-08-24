;;--------------------------------------------------------------------
;; python
;;--------------------------------------------------------------------

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(add-to-list 'interpreter-mode-alist '("python" . python-ts-mode))

(setq python-shell-interpreter "python3")
(setq indent-tabs-mode nil)
(setq python-indent-offset 4)
(setq tab-width 4)

(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'before-save-hook 'python-black-on-save-mode-enable-dwim)

(provide 'init-python)
