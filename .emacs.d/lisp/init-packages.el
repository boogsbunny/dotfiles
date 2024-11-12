;;--------------------------------------------------------------------
;; packages
;;--------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(setq package-selected-packages
      '(add-node-modules-path
        alert
        auth-source-pass
        auto-complete
        bui
        cargo
        circe
        cl-generic
        code-review
        company
        company-box
        company-go
        company-restclient
        dap-mode
        dash
        disk-usage
        dired-du
        dmenu
        docker
        docker-tramp
        docker-compose-mode
        dockerfile-mode
        dumb-jump
        eat
        edit-indirect
        ef-themes
        eglot
        eldoc-box
        elfeed
        elpy
        emms
        emojify
        erc-hl-nicks
        erc-image
        eshell-vterm
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
        go
        guix
        hackernews
        helm
        helm-company
        helm-dash
        helm-descbinds
        helm-emms
        helm-exwm
        helm-flycheck
        helm-go-package
        helm-eww
        helm-ls-git
        helm-notmuch
        helm-org
        helm-org-contacts
        helm-pass
        helm-projectile
        helm-rg
        helm-selector
        helm-sly
        helm-switch-shell
        helm-switch-to-repl
        helm-system-packages
        helm-tramp
        highlight-indent-guides
        hydra
        json-mode
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
        magit
        magit-todos
        markdown-mode+
        modus-themes
        notmuch
        oauth2
        olivetti
        ol-notmuch
        org
        org-alert
        org-contrib
        org-caldav
        org-gcal
        org-journal
        org-modern
        orgit
        org-roam
        org-mime
        password-store
        password-store-otp
        pdf-tools
        pers-projectile
        perspective
        perspective-exwm
        pinentry
        prettier
        prettier-js
        projectile
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
        sly-named-readtables
        smart-mode-line
        svg-tag-mode
        transmission
        tree-sitter
        tree-sitter-langs
        typescript-mode
        websocket
        web-mode
        which-key
        windower
        xelb
        yasnippet
        youtube-dl))

(provide 'init-packages)
