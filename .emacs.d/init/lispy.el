;;--------------------------------;
;; Lispy
;;--------------------------------;

(use-package lispy
  :ensure t)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(use-package lispyville
  :ensure t)

(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(operators
     c-w
     (escape insert)
     mark-toggle
     ))
  (lispyville--define-key '(motion normal visual)
    (kbd "M-h") #'lispyville-previous-opening
    (kbd "M-l") #'lispyville-next-opening
    (kbd "M-H") #'lispy-up-slurp
    (kbd "M-J") #'lispyville-drag-forward
    (kbd "M-K") #'lispyville-drag-backward
    (kbd "M-L") #'lispy-move-right
    (kbd "C-x C-e") #'lispy-eval
    (kbd "C-<return>") #'lispy-join
    (kbd "M-<backspace>") 'lispyville-delete-backward-word
    ")" #'lispy-right)
  (lispyville--define-key 'insert
    (kbd "<backspace>") 'lispy-delete-backward
    (kbd "M-<backspace>") 'lispyville-delete-backward-word
    ";" 'lispy-comment
    "'" 'lispy-tick
    "`" 'lispy-backtick
    "\"" 'lispy-quotes
    "(" 'lispy-parens
    ")" 'lispy-right-nostring))

(add-hook 'lispy-mode-hook #'lispyville-mode)

(use-package rainbow-delimiters
  :ensure t)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
