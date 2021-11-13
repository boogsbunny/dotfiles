;;--------------------------------;
;; Python
;;--------------------------------;

(require 'python-mode nil t)

;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq python-shell-interpreter "python3")

(add-hook 'python-mode-hook #'python-black)

(provide 'init-python)
