;;--------------------------------------------------------------------
;; perspective
;;--------------------------------------------------------------------

(require 'perspective)

(customize-set-variable 'persp-mode-prefix-key (kbd "C-M-w"))
(customize-set-variable 'persp-initial-frame-name "work")
(customize-set-variable 'persp-state-default-file "~/.emacs.d/lisp/persp-state")
(add-hook 'kill-emacs-hook #'persp-state-save)

(setq persp-modestring-short t
      persp-show-modestring 'modeline
      persp-sort 'created)

(persp-mode)

(defun boogs/disable-persp-kill-advice (&rest _)
  (when (advice-member-p #'persp-maybe-kill-buffer 'kill-current-buffer)
    (advice-remove 'kill-current-buffer #'persp-maybe-kill-buffer)))

(add-hook 'persp-mode-hook #'boogs/disable-persp-kill-advice)
(boogs/disable-persp-kill-advice)

(provide 'init-perspective)
