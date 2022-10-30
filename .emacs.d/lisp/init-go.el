;;--------------------------------;
;; Golang
;;--------------------------------;

;;; We should not need to use `use-local-map' here.
;;; Reported at https://github.com/dominikh/go-mode.el/issues/191.
(use-local-map go-mode-map)

(setq-default fill-column (string-to-number "80"))

(boogs/local-set-keys
 "C-c m" 'boogs/go-main
 "C-c D" 'godoc
 "C-c d" 'godoc-at-point
 "M-." #'godef-jump
 )

(when (require 'helm-go-package nil t)
  (local-set-key (kbd "C-c D") 'helm-go-package))

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; (setq gofmt-command "goimports")
(setq godoc-command "godoc -ex")
(setq godoc-and-godef-command "godoc -ex")

(defun boogs/go-set-compile-command ()
  "Set `compile-command' depending on the context.

- go install: file is in GOPATH and is not a test file.
- go test: file is in GOPATH and is a test file.
- go run `buffer-file-name': file is not in GOPATH.

Note that the -cover test flag is left out since it shifts line numbers."
  (interactive)
  (setq compile-command
        (if (boogs/go-buffer-in-gopath-p)
            (if (string-match "_test.[gG][oO]$" buffer-file-name)
                "go test -v -run ."
              "go install")
          (concat "go run " (shell-quote-argument buffer-file-name)))))

(defun boogs/go-buffer-in-gopath-p ()
  (if (not buffer-file-name)
      nil
    (let ((dir (expand-file-name (file-name-directory buffer-file-name)))
          (looping t)
          (gopath (split-string (getenv "GOPATH") ":")))
      (while (progn
               (if (member dir gopath)
                   (setq looping nil)
                 (setq dir (expand-file-name ".." dir)))
               (and looping (not (string= dir "/")))))
      (if (string= dir "/") nil t))))

(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook 'boogs/go-set-compile-command)

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

(add-hook 'go-mode-hook #'tree-sitter-mode)
(add-hook 'go-mode-hook #'tree-sitter-hl-mode)
(add-hook 'go-mode-hook #'company-mode)

(provide 'init-go)
