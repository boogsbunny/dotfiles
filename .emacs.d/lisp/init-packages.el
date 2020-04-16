;;--------------------------------;
;; Packages
;;--------------------------------;

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize))

(setq package-selected-packages
      '(rmsbolt
        auto-complete
        company
        csharp-mode
        docker
        elfeed
        evil
        flycheck
        helm
        ledger-mode
        slime
        sly
        lispy
        lispyville
        rainbow-delimiters
        lsp-mode
        lsp-ui
        company-lsp
        helm-lsp
        which-key
        org
        geiser
        org-superstar
        elpy
        restclient
        rut-mode
        cargo
        powerline
        airline-themes
        magit
        yasnippet
        orgit
        forge
        evil-collection
        evil-magit
        evil-org
        evil-ledger
        org-journal
        apropospriate-theme
        websocket
        alert
        circe
        emojify
        request
        oauth2
        jiralib2
        markdown-mode+
        ox-jira
        language-detection
        ))

(provide 'init-packages)
