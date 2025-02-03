;;--------------------------------------------------------------------
;; styles
;;--------------------------------------------------------------------

(require 'modus-themes)

;;; general
(defun boogs/get-face-attribute-height ()
  (if (laptop-screen-p)
      ;; 120
      ;; 150
      ;; 250
      300
    250))

(set-face-attribute 'default nil
                    :font "Iosevka Term"
                    :width 'normal
                    :height (boogs/get-face-attribute-height))

(set-face-background 'mouse "#777777")  ; darker mouse, less distracting.

;;; when i want to change the font size
(defhydra hydra-zoom (global-map "C-c")
  "zoom"
  ("k" boogs/face-attribute-height-increase "font in")
  ("j" boogs/face-attribute-height-decrease "font out")
  ("K" text-scale-increase "text in")
  ("J" text-scale-decrease "text out")
  ("f" nil "quit" :color blue))

(defun boogs/face-attribute-height-increase ()
  (interactive)
  (setq face-attribute-height (+ face-attribute-height 5))
  (set-face-attribute 'default nil :height face-attribute-height))

(defun boogs/face-attribute-height-decrease ()
  (interactive)
  (setq face-attribute-height (- face-attribute-height 5))
  (set-face-attribute 'default nil :height face-attribute-height))

;;; rainbow-delimiters
(defun set-rainbow-delimiters-faces (colors)
  (with-eval-after-load 'rainbow-delimiters
    (let ((colors colors)) ; capture the colors parameter in a closure
      (set-face-foreground 'rainbow-delimiters-depth-1-face (nth 0 colors))
      (set-face-foreground 'rainbow-delimiters-depth-2-face (nth 1 colors))
      (set-face-foreground 'rainbow-delimiters-depth-3-face (nth 2 colors))
      (set-face-foreground 'rainbow-delimiters-depth-4-face (nth 3 colors))
      (set-face-foreground 'rainbow-delimiters-depth-5-face (nth 4 colors))
      (set-face-foreground 'rainbow-delimiters-depth-6-face (nth 5 colors))
      (set-face-foreground 'rainbow-delimiters-depth-7-face (nth 6 colors))
      (set-face-foreground 'rainbow-delimiters-depth-8-face (nth 7 colors))
      (set-face-foreground 'rainbow-delimiters-depth-9-face (nth 8 colors))
      (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                          :foreground 'unspecified
                          :inherit 'error
                          :strike-through t))))

(defun set-rainbow-delimiters-for-modus-operandi-tinted ()
  "Set rainbow delimiters for the modus-operandi-tinted theme."
  (set-rainbow-delimiters-faces '("#000000" "#005f00" "#007f7f" "#870000" "#875f00"
                                  "#5f00af" "#d75f00" "#005f87" "#5f5f5f")))

(defun set-rainbow-delimiters-for-modus-vivendi-tinted ()
  "Set rainbow delimiters for the modus-vivendi-tinted theme."
  (set-rainbow-delimiters-faces '("#ffffff" "#44bc44" "#00d3d0" "#ff8059" "#ff9977"
                                  "#efdf00" "#2fafff" "#00c06f" "#989898")))

;;; helm
(defun set-helm-faces (colors)
  (with-eval-after-load 'helm
    (let ((colors colors)) ; capture the colors parameter in a closure
      (set-face-attribute 'helm-source-header nil
                          :inherit 'header-line
                          :height 'unspecified
                          :background (cdr (assoc 'background colors))
                          :foreground (cdr (assoc 'foreground colors)))
      (set-face-background 'helm-selection (cdr (assoc 'selection colors)))
      (set-face-background 'helm-visible-mark (cdr (assoc 'visible-mark-bg colors)))
      (set-face-foreground 'helm-visible-mark (cdr (assoc 'visible-mark-fg colors)))
      (set-face-foreground 'helm-match (cdr (assoc 'match colors)))
      (set-face-attribute 'helm-buffer-directory nil
                          :background 'unspecified
                          :foreground (cdr (assoc 'directory-fg colors))
                          :weight 'bold)
      (set-face-attribute 'helm-ff-directory nil
                          :background 'unspecified
                          :foreground 'unspecified
                          :weight 'unspecified
                          :inherit 'helm-buffer-directory)
      (set-face-attribute 'helm-ff-file nil
                          :background 'unspecified
                          :foreground 'unspecified
                          :weight 'unspecified
                          :inherit 'helm-buffer-file)
      (set-face-foreground 'helm-grep-finish (cdr (assoc 'grep-finish colors))))))

(defun set-helm-for-modus-operandi-tinted ()
  "Configure Helm faces for the modus-operandi-tinted theme."
  (set-helm-faces '((background "#fbf7f0")
                    (foreground "#000000")
                    (selection "#d7d7d7")
                    (visible-mark-bg "#e0e0e0")
                    (visible-mark-fg "#000000")
                    (match "#a60000")
                    (directory-fg "#005f00")
                    (grep-finish "#00AA00"))))

(defun set-helm-for-modus-vivendi-tinted ()
  "Configure Helm faces for the modus-vivendi-tinted theme."
  (set-helm-faces '((background "#0d0e1c")
                    (foreground "#ffffff")
                    (selection "#4f4f4f")
                    (visible-mark-bg "#2f2f2f")
                    (visible-mark-fg "#ffffff")
                    (match "#ff8059")
                    (directory-fg "#2fafff")
                    (grep-finish "#44bc44"))))

;;; helm-ls-git
(defun set-helm-ls-git-faces-for-modus-operandi-tinted ()
  (with-eval-after-load 'helm-ls-git
    (custom-set-faces
     '(helm-ls-git-modified-not-staged-face ((t (:foreground "#7a4f2f"))))
     '(helm-ls-git-modified-and-staged-face ((t (:foreground "#80601f"))))
     '(helm-ls-git-renamed-modified-face ((t (:foreground "#80601f"))))
     '(helm-ls-git-untracked-face ((t (:foreground "#a60000"))))
     '(helm-ls-git-added-copied-face ((t (:foreground "#006800"))))
     '(helm-ls-git-added-modified-face ((t (:foreground "#3548cf"))))
     '(helm-ls-git-deleted-not-staged-face ((t (:foreground "#7f0000"))))
     '(helm-ls-git-deleted-and-staged-face ((t (:foreground "#595959"))))
     '(helm-ls-git-conflict-face ((t (:foreground "#a0132f"))))
     '(helm-ls-git-branches-current ((t (:foreground "#80601f" :weight bold))))
     '(helm-ls-git-branches-name ((t (:foreground "#a60000"))))
     '(helm-ls-git-branches-name-current ((t (:foreground "#006800" :weight bold)))))))

(defun set-helm-ls-git-faces-for-modus-vivendi-tinted ()
  (with-eval-after-load 'helm-ls-git
    (custom-set-faces
     '(helm-ls-git-modified-not-staged-face ((t (:foreground "#dfaf7a"))))
     '(helm-ls-git-modified-and-staged-face ((t (:foreground "#d2b580"))))
     '(helm-ls-git-renamed-modified-face ((t (:foreground "#d2b580"))))
     '(helm-ls-git-untracked-face ((t (:foreground "#ff5f59"))))
     '(helm-ls-git-added-copied-face ((t (:foreground "#44bc44"))))
     '(helm-ls-git-added-modified-face ((t (:foreground "#79a8ff"))))
     '(helm-ls-git-deleted-not-staged-face ((t (:foreground "#ff9f80"))))
     '(helm-ls-git-deleted-and-staged-face ((t (:foreground "#989898"))))
     '(helm-ls-git-conflict-face ((t (:foreground "#ff7f9f"))))
     '(helm-ls-git-branches-current ((t (:foreground "#fec43f" :weight bold))))
     '(helm-ls-git-branches-name ((t (:foreground "#ff5f59"))))
     '(helm-ls-git-branches-name-current ((t (:foreground "#44bc44" :weight bold)))))))

;;; org
(defun set-org-faces (colors)
  (let* ((variable-tuple
          (cond ((x-list-fonts "Iosevka Comfy") '(:font "Iosevka Comfy"))
                ((x-family-fonts "Sans Serif")   '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font. Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-set-faces
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
     `(variable-pitch ((t (:family "Iosevka Comfy" :height 280 :weight thin))))
     `(fixed-pitch ((t (:family "Fira Code Retina" :height 250))))
     `(org-block ((t (:inherit fixed-pitch :background ,(cdr (assoc 'background-dim colors))))))
     `(org-code ((t (:inherit (shadow fixed-pitch) :background ,(cdr (assoc 'background-dim colors))))))
     `(org-document-info ((t (:foreground ,(cdr (assoc 'foreground-dim colors))))))
     `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch) :foreground ,(cdr (assoc 'foreground-dim colors))))))
     `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     `(org-link ((t (:foreground ,(cdr (assoc 'blue colors)) :underline t))))
     `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch) :foreground ,(cdr (assoc 'foreground-dim colors))))))
     `(org-property-value ((t (:inherit fixed-pitch))) t)
     `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch) :foreground ,(cdr (assoc 'foreground-dim colors))))))
     `(org-table ((t (:inherit fixed-pitch :foreground ,(cdr (assoc 'blue colors))))))
     `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     `(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
    (setf org-todo-keyword-faces
          `(("TODO" . (:foreground ,(cdr (assoc 'red colors)) :weight bold))
            ("NEXT" . (:foreground ,(cdr (assoc 'orange colors)) :weight bold))
            ("WAITING" . (:foreground ,(cdr (assoc 'yellow colors)) :weight bold))
            ("DELEGATED" . (:foreground ,(cdr (assoc 'blue colors)) :weight bold))
            ("SOMEDAY" . (:foreground ,(cdr (assoc 'cyan colors)) :weight bold))
            ("DONE" . (:foreground ,(cdr (assoc 'green colors)) :weight bold))
            ("CANCELED" . (:foreground ,(cdr (assoc 'magenta colors)) :weight bold))))
    (setf org-modern-todo-faces
          `(("TODO" . (:background ,(cdr (assoc 'red colors)) :foreground "white" :weight bold))
            ("NEXT" . (:background ,(cdr (assoc 'orange colors)) :foreground "white" :weight bold))
            ("WAITING" . (:background ,(cdr (assoc 'yellow colors)) :foreground "white" :weight bold))
            ("DELEGATED" . (:background ,(cdr (assoc 'blue colors)) :foreground "white" :weight bold))
            ("SOMEDAY" . (:background ,(cdr (assoc 'cyan colors)) :foreground "white" :weight bold))
            ("DONE" . (:background ,(cdr (assoc 'green colors)) :foreground "white" :weight bold))
            ("CANCELED" . (:background ,(cdr (assoc 'magenta colors)) :foreground "white" :weight bold))))))

(defun set-org-faces-for-modus-operandi-tinted ()
  "Configure Org faces for the modus-operandi-tinted theme."
  (set-org-faces '((red . "#a60000")
                   (orange . "#804000")
                   (yellow . "#705000")
                   (green . "#005f00")
                   (blue . "#0031a9")
                   (cyan . "#005e8b")
                   (magenta . "#721045")
                   (background-main . "#fbf7f0")
                   (background-dim . "#efe9dd")
                   (foreground-main . "#000000")
                   (foreground-dim . "#595959"))))

(defun set-org-faces-for-modus-vivendi-tinted ()
  "Configure Org faces for the modus-vivendi-tinted theme."
  (set-org-faces '((red . "#ff8059")
                   (orange . "#ff9977")
                   (yellow . "#efdf00")
                   (green . "#44bc44")
                   (blue . "#2fafff")
                   (cyan . "#00d3d0")
                   (magenta . "#feacd0")
                   (background-main . "#0d0e1c")
                   (background-dim . "#1d2235")
                   (foreground-main . "#ffffff")
                   (foreground-dim . "#989898"))))

;;; transparency
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

(defun run-gsettings-safely (is-dark)
  "Run gsettings commands safely to switch theme mode.
IS-DARK should be non-nil for dark mode, nil for light mode."
  (let ((scheme (if is-dark "prefer-dark" "prefer-light"))
        (theme (if is-dark "Adwaita-dark" "Adwaita")))
    (dolist (cmd `(("set" "org.gnome.desktop.interface" "color-scheme" ,scheme)
                   ("set" "org.gnome.desktop.interface" "gtk-theme" ,theme)
                   ("set" "org.gnome.desktop.interface" "prefer-dark" ,(if is-dark "true" "false"))))
      (condition-case err
          (apply #'call-process "gsettings" nil nil nil cmd)
        (error
         (message "Warning: couldn't set gsettings theme: %s"
                  (error-message-string err)))))
    ;; Add small delay to allow changes to propagate
    (sleep-for 0.5)))

(defun run-kde-theme-safely (is-dark)
  "Run KDE commands safely to switch theme mode.
IS-DARK should be non-nil for dark mode, nil for light mode."
  (let* ((look-and-feel (if is-dark "org.kde.breezedark.desktop" "org.kde.breeze.desktop"))
         (commands
          `(("plasma-apply-lookandfeel" "-a" ,look-and-feel)
            ("plasma-apply-desktoptheme" ,(if is-dark "breeze-dark" "breeze-light")))))

    (dolist (cmd commands)
      (when (executable-find (car cmd))
        (condition-case err
            (apply #'call-process (car cmd) nil nil nil (cdr cmd))
          (error
           (message "Warning: couldn't set KDE theme command %s: %s"
                    (car cmd) (error-message-string err))))))
    (sleep-for 0.5)))

(defun switch-theme-based-on-time ()
  "Switch between 'modus-operandi-tinted' and 'modus-vivendi-tinted' based on the time of day."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (and (>= hour 7) (< hour 20)) ; from 7:00 to 19:59 use 'modus-operandi-tinted, then 'moduse-vivendi-tinted
        (progn
          ;; (run-gsettings-safely nil)
          (run-kde-theme-safely nil)
          (load-theme 'modus-operandi-tinted t)
          (set-helm-ls-git-faces-for-modus-operandi-tinted)
          (set-org-faces-for-modus-operandi-tinted)
          ;; TODO: fix closure since color variable is void
          ;; (set-rainbow-delimiters-for-modus-operandi-tinted)
          ;; (set-helm-for-modus-operandi-tinted)
          )
      (progn
        ;; (run-gsettings-safely t)
        (run-kde-theme-safely t)
        (load-theme 'modus-vivendi-tinted t)
        (set-helm-ls-git-faces-for-modus-vivendi-tinted)
        (set-org-faces-for-modus-vivendi-tinted)
        ;; TODO: fix closure since color variable is void
        ;; (set-rainbow-delimiters-for-modus-operandi-tinted)
        ;; (set-helm-for-modus-vivendi-tinted)
        ))))

;;; run the theme switch function at the start
(switch-theme-based-on-time)

(run-at-time "07:00" (* 24 60 60)'switch-theme-based-on-time) ; switch to day theme at 7:00 AM
(run-at-time "20:00" (* 24 60 60)'switch-theme-based-on-time) ; switch to night theme at 8:00 PM

(provide 'init-styles)
