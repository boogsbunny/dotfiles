;;--------------------------------;
;; Helm
;;--------------------------------;

(setq
 helm-follow-mode-persistent t
 helm-reuse-last-window-split-state t
 helm-display-header-line nil
 helm-findutils-search-full-path t
 helm-show-completion-display-function nil
 helm-completion-mode-string ""
 helm-dwim-target 'completion
 helm-echo-input-in-header-line t
 helm-use-frame-when-more-than-two-windows nil
 helm-grep-save-buffer-name-no-confirm t

 helm-M-x-fuzzy-match t
 helm-apropos-fuzzy-match t
 helm-buffers-fuzzy-matching t
 helm-completion-in-region-fuzzy-match t
 helm-eshell-fuzzy-match t
 helm-imenu-fuzzy-match t
 helm-locate-library-fuzzy-match t
 helm-recentf-fuzzy-match t

 ;; https://github.com/emacs-helm/helm/issues/1910
 helm-buffers-end-truncated-string "…"
 helm-buffer-max-length 22

 helm-window-show-buffers-function 'helm-window-mosaic-fn
 ;; helm-window-prefer-horizontal-split t
 )

(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap list-buffers] 'helm-mini)
(global-set-key [remap apropos-command] 'helm-apropos)
(global-set-key [remap query-replace-regexp] 'helm-regexp)


(when (require 'helm-ls-git nil t)
  (setq helm-ls-git-fuzzy-match t)
  ;; `helm-source-ls-git' must be defined manually.
  ;; See https://github.com/emacs-helm/helm-ls-git/issues/34.
  (setq helm-source-ls-git
        (and (memq 'helm-source-ls-git helm-ls-git-default-sources)
             (helm-make-source "Git files" 'helm-ls-git-source
               :fuzzy-match helm-ls-git-fuzzy-match))))

(helm-mode 1)

(dired-async-mode)

(provide 'init-helm)
