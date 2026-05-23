;;--------------------------------------------------------------------
;; consult-gh
;;--------------------------------------------------------------------

(require 'consult-gh nil t)

(setq consult-gh-default-clone-directory "~/repos"
      consult-gh-show-preview t
      consult-gh-preview-key "C-o"
      consult-gh-repo-action #'consult-gh--repo-browse-files-action
      consult-gh-default-interactive-command #'consult-gh-transient
      consult-gh-large-file-warning-threshold 2500000
      consult-gh-confirm-before-clone t)

;; Persist known orgs/repos
(add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
(add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)

;; Default keybindings (M-s g etc.)
(consult-gh-enable-default-keybindings)

(require 'consult-gh-transient)

(require 'consult-gh-embark)
(consult-gh-embark-mode 1)

(require 'consult-gh-forge)
(consult-gh-forge-mode 1)

(require 'consult-gh-nerd-icons)

(global-set-key (kbd "C-c g t") #'consult-gh-transient)
(global-set-key (kbd "C-c g s") #'consult-gh-search-prs)
(global-set-key (kbd "C-c g r") #'consult-gh-search-repos)

(provide 'init-consult-gh)
