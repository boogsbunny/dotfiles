;;--------------------------------------------------------------------
;; golang
;;--------------------------------------------------------------------

(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\.mod\\'" . go-mod-ts-mode))

(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(setq gofmt-command "goimports")

(defun boogs/go-format-on-save ()
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (gofmt)))

(defun boogs/go-enable-format-on-save ()
  (add-hook 'before-save-hook #'boogs/go-format-on-save nil t))

(add-hook 'go-mode-hook #'boogs/go-enable-format-on-save)
(add-hook 'go-ts-mode-hook #'boogs/go-enable-format-on-save)

(add-hook 'go-ts-mode-hook (function (lambda () (setq fill-column 120))))

(defun boogs/go-setup ()
  (setq tab-width 8))
(add-hook 'go-ts-mode-hook #'boogs/go-setup)

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

(define-key go-mode-map (kbd "C-c m") #'boogs/go-main)

(flycheck-define-checker go-build-escape
  "A Go escape checker using `go build -gcflags -m'."
  :command ("go" "build" "-gcflags" "-m"
            (option-flag "-i" flycheck-go-build-install-deps)
            ;; multiple tags are listed as "dev debug ..."
            (option-list "-tags=" flycheck-go-build-tags concat)
            "-o" null-device)
  :error-patterns
  ((warning line-start (file-name) ":" line ":"
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
         line-end))
  :modes go-ts-mode
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name))))))

(with-eval-after-load 'flycheck
  (add-to-list 'flycheck-checkers 'go-build-escape)
  (flycheck-add-next-checker 'go-gofmt 'go-build-escape))

(with-eval-after-load 'dape
  (setq dape-request-timeout 120)
  (add-to-list 'dape-configs
               '(dlv-go-test-local
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd (if buffer-file-name
                                 (file-name-directory buffer-file-name)
                               default-directory)
                 port :autoport
                 :request "launch"
                 :type "go"
                 :mode "test"
                 :cwd "."
                 :program "."
                 :args [])))

(provide 'init-go)
