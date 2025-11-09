;;--------------------------------------------------------------------;
;; version control
;;--------------------------------------------------------------------;
(require 'difftastic)
(require 'difftastic-bindings)
(difftastic-bindings-mode)


(setq auto-revert-mode-text "")
;; (set-face-foreground 'magit-branch-remote "orange red")
(setq git-commit-summary-max-length fill-column)
;; (define-key magit-mode-map (kbd "s-<tab>") nil)
(setq magit-diff-refine-hunk 'all)
(setq magit-repository-directories '(("~/.password-store")
                                     ("~/common-lisp" . 1)
                                     ("~/repos" . 1)
                                     ("~/projects" . 9)))

(setq magit-log-margin '(t age magit-log-margin-width t 18))

(when (require 'magit-todos nil 'noerror)
  (magit-todos-mode))

(setq magit-todos-exclude-globs '(".git/" "*.har" "*.map" "*.html"))

(setq magit-todos-keywords-list
      '("BUG"
        "DEPRECATED"
        "DONT"
        "FAIL"
        "FIXME"
        "HACK"
        "HOLD"
        "KLUDGE"
        "NEXT"
        "NOTE"
        "OKAY"
        "PROG"
        "REVIEW"
        "TEMP"
        "THEM"
        "TODO"
        "XXX+"))

(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(setq forge-add-default-bindings nil)
(require 'forge nil 'noerror)
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

(with-eval-after-load 'orgit
  (setq orgit-store-repository-id t))

(provide 'init-magit)
