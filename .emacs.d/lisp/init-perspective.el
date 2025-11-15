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

(defun boogs/persp-unadvise-kill-buffer (&rest _)
	"Remove Perspective's heavy kill-buffer advice.
This prevents expensive window reconfiguration during cleanup
and normal buffer kills."
	(dolist (fn '(kill-buffer kill-current-buffer))
		(when (advice-member-p #'persp-maybe-kill-buffer fn)
			(advice-remove fn #'persp-maybe-kill-buffer))))

(add-hook 'persp-mode-hook #'boogs/persp-unadvise-kill-buffer)

(persp-mode)

(provide 'init-perspective)
