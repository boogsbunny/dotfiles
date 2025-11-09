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

(require 'consult)
(consult-customize consult--source-buffer :hidden t :default nil)
(add-to-list 'consult-buffer-sources persp-consult-source)

(persp-mode)

(provide 'init-perspective)
