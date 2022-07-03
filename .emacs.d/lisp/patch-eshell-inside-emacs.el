;;; Reported upstream, see #39596.
(defun boogs/set-inside-emacs ()
  "Export INSIDE_EMACS just like M-x shell does.
This is useful for programs like Guix that take provisions for Emacs."
  (setenv "INSIDE_EMACS" (format "%s;%s" emacs-version "eshell")))

(add-hook 'eshell-mode-hook 'boogs/set-inside-emacs)

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(provide 'patch-eshell-inside-emacs)
