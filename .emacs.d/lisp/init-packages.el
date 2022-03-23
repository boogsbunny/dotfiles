;;--------------------------------;
;; Packages
;;--------------------------------;

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize))

(setq package-selected-packages
      '(
        alert
        apropospriate-theme
        auth-source-pass
        auto-complete
        cargo
        circe
        cl-generic
        color-theme-sanityinc-tomorrow
        company
        company-lsp
        company-go
        company-restclient
        csharp-mode
        dash
        diminish
        docker
        docker-compose-mode
        dockerfile-mode
        dumb-jump
        doom-themes
        elfeed
        elpy
        emms
        emojify
        evil
        evil-collection
        evil-commentary
        evil-escape
        evil-ledger
        evil-magit
        evil-multiedit
        evil-org
        exwm
        flycheck
        forge
        geiser
        geiser-guile
        go-eldoc
        go-guru
        gruvbox-theme
        guix
        hackernews
        helm
        helm-company
        helm-dash
        helm-descbinds
        helm-flycheck
        helm-go-package
        helm-eww
        helm-jira
        helm-ls-git
        helm-lsp
        helm-notmuch
        helm-org
        helm-org-contacts
        helm-pass
        helm-selector
        helm-sly
        helm-system-packages
        highlight-indent-guides
        jiralib2
        js2-mode
        kubernetes
        kubernetes-evil
        kubernetes-helm
        kubernetes-tramp
        language-detection
        ledger-mode
        leetcode
        lispy
        lispyville
        lsp-mode
        lsp-ui
        magit
        magit-todos
        markdown-mode+
        notmuch
        oauth2
        org
        org-caldav
        org-gcal
        org-jira
        org-journal
        orgit
        org-roam
        pdf-tools
        pinentry
        prettier
        prettier-js
        python-black
        rainbow-delimiters
        rainbow-mode
        request
        restclient
        rjsx-mode
        rmsbolt
        rust-mode
        slack
        sly
        sly-asdf
        sly-macrostep
        sly-quicklisp
        solarized-theme
        smart-mode-line
        transmission
        typescript-mode
        websocket
        web-mode
        which-key
        windower
        xelb
        yasnippet
        youtube-dl
        ))

(provide 'init-packages)
