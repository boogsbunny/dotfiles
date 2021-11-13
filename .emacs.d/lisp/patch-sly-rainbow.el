;; Workaround for https://github.com/joaotavora/sly/issues/452.
(defun boogs/disable-rainbow-delimiters (_orig &rest args)
  "`rainbow-delimiters-mode' interferes with `ffap' (and thus Helm) in SLY mrepl.
Example:

> (princ \"/home\")
/home
\"/home\"

Running `ffap' on the first /home would not work as expected."
  (rainbow-delimiters-mode -1))
(advice-add 'sly-mrepl-return :before 'boogs/disable-rainbow-delimiters)

(defun boogs/enable-rainbow-delimiters (_orig &rest args)
  "See `boogs/disable-rainbow-delimiters'."
  (rainbow-delimiters-mode 1))
(advice-add 'sly-mrepl--insert-prompt :before 'boogs/enable-rainbow-delimiters)

(provide 'patch-sly-rainbow)
