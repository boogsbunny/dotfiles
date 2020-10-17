;;--------------------------------;
;; Version Control
;;--------------------------------;

(setq auto-revert-mode-text "")
(set-face-foreground 'magit-branch-remote "orange red")
(setq git-commit-summary-max-length fill-column)
(define-key magit-mode-map (kbd "s-<tab>") nil)
(setq magit-diff-refine-hunk 'all)
(setq magit-repository-directories '(("~/projects" . 1)
                                     ("~/Repos" . 1)
																		 ("~/.local/share/emacs/site-lisp" . 1))

(when (require 'magit-todos nil 'noerror)
  (magit-todos-mode))

(setq magit-todos-exclude-globs '("*.map" "*.html"))

(require 'forge nil 'noerror)
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

(with-eval-after-load 'orgit
  (setq orgit-store-repository-id t))

(provide 'init-magit)
