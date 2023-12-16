;;--------------------------------------------------------------------
;; python
;;--------------------------------------------------------------------

(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq python-shell-interpreter "python3")

(setq indent-tabs-mode nil)
(setq python-indent-offset 4)
(setq tab-width 4)

(elpy-enable)
(add-hook 'python-mode-hook #'elpy-mode)
(setq elpy-rpc-python-command "python3")

(add-hook 'python-mode-hook #'python-black-on-save-mode)

(provide 'init-python)
