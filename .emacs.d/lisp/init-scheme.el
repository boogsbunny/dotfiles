;;; Scheme

(require 'init-lispy)

;; REVIEW: Geiser REPL support is missing.
;; See https://github.com/abo-abo/lispy/issues/542.
(add-to-list 'lispy-goto-symbol-alist '(geiser-repl-mode lispy-goto-symbol-scheme le-scheme))

;; (add-hook 'scheme-mode-hook 'boogs/turn-on-complete-filename)
;; (add-hook 'scheme-mode-hook 'boogs/turn-on-tab-width-to-8) ; Because some existing code uses tabs.
;; (add-hook 'scheme-mode-hook 'boogs/turn-off-indent-tabs)   ; Should not use tabs.
(add-hook 'scheme-mode-hook 'boogs/init-lispy)
(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

(with-eval-after-load 'scheme
  (when (require 'patch-scheme nil t)
    (setq scheme-imenu-generic-expression
          al/scheme-imenu-generic-expression)
    (advice-add 'scheme-indent-function
                :override 'al/scheme-indent-function)
    (add-hook 'scheme-mode-hook 'al/scheme-fix-docstring-font-lock)))

(with-eval-after-load 'geiser-repl
  (when (require 'helm nil :noerror)
    (defun boogs/helm/geiser-set-keys ()
      (define-key geiser-repl-mode-map (kbd "M-p") 'helm-comint-input-ring))
    (add-hook 'geiser-repl-mode-hook 'boogs/helm/geiser-set-keys)))

(when (require 'geiser-impl nil 'noerror)
  ;; (setq geiser-repl-skip-version-check-p t
  ;;       geiser-mode-start-repl-p t)
  (setq geiser-active-implementations (delq 'chicken geiser-active-implementations)
        geiser-default-implementation 'guile
        ;; geiser-repl-save-debugging-history-p t
        geiser-repl-history-size 5000)
  (add-hook 'geiser-repl-mode-hook 'lispyville-mode)
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)

  ;; If auto-compiling is too cumbersome in the REPL, consider disabling
  ;; auto-compilation.  See https://gitlab.com/emacs-geiser/geiser/-/issues/11.
  ;; (setq geiser-guile-binary '("guile" "--no-auto-compile"))

  ;; Fontify debug buffer:
  (add-hook 'geiser-debug-mode-hook 'scheme-mode) ; TODO: Does not work?

  (with-eval-after-load 'geiser-repl
    (define-key geiser-repl-mode-map (kbd "C-c m") 'switch-to-geiser-module)
    (define-key geiser-repl-mode-map (kbd "C-c C-m") nil)
    ;; Bind same macro expansion keys in the REPL:
    (define-key geiser-repl-mode-map (kbd "C-c C-m C-e") 'geiser-expand-last-sexp)
    (define-key geiser-repl-mode-map (kbd "C-c C-m C-x") 'geiser-expand-definition)
    (define-key geiser-repl-mode-map (kbd "C-c C-m C-r") 'geiser-expand-region)))

(provide 'init-scheme)
