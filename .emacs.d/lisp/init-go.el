;;--------------------------------------------------------------------
;; golang
;;--------------------------------------------------------------------

(add-to-list 'exec-path "~/go/bin")
(setq gofmt-command "goimports")

(add-hook 'go-mode-hook (function (lambda () (setq olivetti-body-width 140))))
(add-hook 'go-mode-hook (function (lambda () (setq fill-column 120))))

(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook #'tree-sitter-hl-mode)

;; eglot
(add-hook 'go-mode-hook 'eglot-ensure)

(defun boogs/go-setup ()
  (setq tab-width 8))
(add-hook 'go-mode-hook #'boogs/go-setup)

(define-skeleton boogs/go-main
  "Insert main function with basic includes."
  nil
  > "package main" "\n" \n
  "import (" \n
  "\"fmt\"" \n
  ")" > "\n" \n
  "func main() {" \n
  > @ _ \n
  "}" > \n)

(boogs/local-set-keys "C-c m" 'boogs/go-main)

(when (require 'helm-go-package nil t)
  (local-set-key (kbd "C-c D") 'helm-go-package))

(flycheck-define-checker go-build-escape
  "A Go escape checker using `go build -gcflags -m'."
  :command ("go" "build" "-gcflags" "-m"
            (option-flag "-i" flycheck-go-build-install-deps)
            ;; multiple tags are listed as "dev debug ..."
            (option-list "-tags=" flycheck-go-build-tags concat)
            "-o" null-device)
  :error-patterns
  (
   (warning line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline) "escapes to heap")
          line-end)
   (warning line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message "moved to heap:" (one-or-more not-newline))
          line-end)
   (info line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message "inlining call to " (one-or-more not-newline))
          line-end)
  )
  :modes go-mode
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))\
  )

(with-eval-after-load 'flycheck
   (add-to-list 'flycheck-checkers 'go-build-escape)
   (flycheck-add-next-checker 'go-gofmt 'go-build-escape))

(provide 'init-go)
