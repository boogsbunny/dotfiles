;;--------------------------------------------------------------------
;; helm
;;--------------------------------------------------------------------

(require 'cl-lib)

;;; TODO: helm-ff should allow opening several marks externally, e.g.  sxiv for
;;; pics. See
;;; https://github.com/emacs-helm/helm/wiki/Find-Files#open-files-externally.
;;; What about the default program?  It currently defaults to ~/.mailcap, which is
;;; not so customizable.  Would ranger's rifle be useful here?  See
;;; https://github.com/emacs-helm/helm/issues/1796.  There is the `openwith' package.

;;; TODO: Batch-open torrent files automatically.  Add to mailcap?  Same as
;;; above, C-c C-x does not allow for opening several files at once.

;;; TODO: helm-find in big folders sometimes leads bad results, like exact match
;;; not appearing first. Better sorting?

;;; TODO: Implement alternating-color multiline lists.
;;; See https://github.com/emacs-helm/helm/issues/1790.

(setq helm-move-to-line-cycle-in-source nil)

(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key (kbd "C-x C-q"))
  (add-hook 'wgrep-setup-hook #'wgrep-change-to-wgrep-mode))

(when (require 'helm-ls-git nil t)
  (setq helm-ls-git-fuzzy-match t)
  ;; `helm-source-ls-git' must be defined manually.
  ;; See https://github.com/emacs-helm/helm-ls-git/issues/34.
  (setq helm-source-ls-git
        (and (memq 'helm-source-ls-git helm-ls-git-default-sources)
             (helm-make-source "Git files" 'helm-ls-git-source
               :fuzzy-match helm-ls-git-fuzzy-match))))

;; faster
(setq helm-grep-git-grep-command
      (concat "rg"
              " --color=always"
              " --smart-case"
              " --no-heading"
              " --line-number %c"
              " --column"
              " --hidden"
              " --glob \"!.git/\""
              " -e %p %f"))

(setq helm-mode-line-string nil)
(helm-mode 1)
;; (helm-autoresize-mode 1)
;; (setq helm-autoresize-max-height 30)
;; (setq helm-autoresize-min-height 30)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)


;;; This makes the copy and rename operations asynchronous.
(dired-async-mode)

;;; Generic configuration.
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

 ;; To prevent M-s f from directly going to symbol at point if in same buffer.
 helm-imenu-execute-action-at-once-if-one nil

 ;; Use woman instead of man.
 ;; helm-man-or-woman-function nil

 ;; https://github.com/emacs-helm/helm/issues/1910
 helm-buffers-end-truncated-string "…"
 helm-buffer-max-length 22

 ;; Default needs special font
 helm-ff-cache-mode-lighter " ⚒"

 helm-window-show-buffers-function 'helm-window-mosaic-fn
 helm-window-prefer-horizontal-split nil
 helm-locate-command "locate %s -e -A --regex %s")

; Helm cache tends to run out-of-sync to frequently when set to 'all.
;(helm-ff-cache-mode-remove-hooks) ; Just setting `helm-ff-keep-cached-candidates' to nil does not seem to remove the hook. Upstream bug?
(setq helm-ff-keep-cached-candidates nil
      helm-ff-cache-mode nil)

(defun boogs/helm-split-window-combined-fn (window)
  "Helm window splitting that combined most standard features.

- With C-u, split inside. With C-u C-u, use same window.
- Else use biggest other window when available.
- Else split horizontally if width>height, vertically otherwise."
  (cond
   ((or (minibufferp helm-current-buffer)
        (and
         (not (one-window-p t))
         (not (equal current-prefix-arg '(4)))
         (not (equal current-prefix-arg '(16)))))
    ;; Find biggest window.
    (let (biggest (maxarea 0))
      (dolist (w (window-list))
        (unless (eq w (selected-window))
          (let ((area (* (window-pixel-width w) (window-pixel-height w))))
            (when (> area maxarea)
              (setq maxarea area
                    biggest w)))))
      biggest))
   ((equal current-prefix-arg '(16))
    ;; Same window.
    (selected-window))
   (t
    ;; If split inside or if unique window.
    (split-window (selected-window) nil
                  (if (> (window-pixel-width) (window-pixel-height))
                      'right
                    'below)))))

(setq helm-always-two-windows nil
      helm-display-buffer-default-height 13
      helm-default-display-buffer-functions '(display-buffer-in-atom-window))

(setq helm-split-window-preferred-function 'boogs/helm-split-window-combined-fn)

(minibuffer-depth-indicate-mode 1)

;;; Add bindings to `helm-apropos`. See
;;; https://github.com/emacs-helm/helm/issues/1140.
(defun boogs/helm-def-source--emacs-commands (&optional default)
  (helm-build-in-buffer-source "Commands"
    :init `(lambda ()
             (helm-apropos-init 'commandp ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :candidate-transformer 'helm-M-x-transformer-1
    :nomark t
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-function))
    :persistent-help "Toggle describe command"
    :action '(("Describe function" . helm-describe-function)
              ("Find function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))
(advice-add 'helm-def-source--emacs-commands :override 'boogs/helm-def-source--emacs-commands)

;;; Make `helm-mini' almighty.
(require 'helm-bookmark)
(setq helm-mini-default-sources `(helm-source-buffers-list
                                  helm-source-recentf
                                  ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                  helm-source-bookmarks
                                  helm-source-bookmark-set
                                  helm-source-buffer-not-found))

;;; Comint
(defun boogs/helm/comint-set-keys ()
  (define-key comint-mode-map (kbd "M-s f") 'helm-comint-prompts-all)
  (define-key comint-mode-map (kbd "M-p") 'helm-comint-input-ring))
(add-hook 'comint-mode-hook 'boogs/helm/comint-set-keys)

;;; TODO: Use helm-ff history in helm file completion.
;;; https://github.com/emacs-helm/helm/issues/1118
;; (define-key helm-read-file-map (kbd "M-p") 'helm-ff-run-switch-to-history)

;; faster to use ripgrep
(setq helm-grep-ag-command (concat "rg"
                                   " --color=never"
                                   " --smart-case"
                                   " --no-heading"
                                   " --line-number %s %s %s")
      helm-grep-file-path-style 'relative)

(setq helm-grep-default-command
      (concat "rg"
              " --color=always"
              " --smart-case"
              " --no-heading"
              " --line-number %e -H -n %p %f"))

(setq helm-grep-default-recurse-command
      (concat "rg"
              " --color=always"
              " --smart-case"
              " --no-heading"
              " --line-number %e -H -n %p %f"))

(defun boogs/helm-grep-git-or-ag (arg)
  "Run `helm-grep-do-git-grep' if possible; fallback to `helm-do-grep-ag' otherwise.
Requires `call-process-to-string' from `functions'."
  (interactive "P")
  (require 'vc)
  (require 'functions)
  (if (and (vc-find-root default-directory ".git")
           (or arg (split-string (boogs/call-process-to-string "git" "ls-files" "-z") "\0" t)))
      ;; faster
      (helm-do-grep-ag arg)
      ;; (helm-grep-do-git-grep arg)
    (helm-do-grep-ag nil)))

(defun boogs/helm-grep-git-all-or-ag ()
  "Run `helm-grep-do-git-grep' over all git files."
  (interactive)
  ;; faster
  (helm-grep-do-git-grep t)
  ;; (helm-do-grep-ag nil)
  )

(defun boogs/helm-mark-or-exchange-rect ()
  "Run `helm-all-mark-rings-before-mark-point' or `rectangle-exchange-point-and-mark' if in rectangle-mark-mode."
  (interactive)
  (if rectangle-mark-mode
      (rectangle-exchange-point-and-mark)
    (helm-all-mark-rings)))

;; (global-set-key [remap execute-extended-command] 'helm-M-x)
;; (global-set-key [remap find-file] 'helm-find-files)
;; (global-set-key [remap occur] 'helm-occur)
;; (global-set-key [remap bookmark-jump] 'helm-filtered-bookmarks)
;; (global-set-key [remap bookmark-set] 'helm-filtered-bookmarks)
;; (global-set-key [remap list-buffers] 'helm-mini)
;; ;; (global-set-key [remap dabbrev-expand] 'helm-dabbrev)
;; (global-set-key [remap yank-pop] 'helm-show-kill-ring)
;; ;;; Do not remap 'exchange-point-and-mark, Evil needs it in visual mode.
;; (global-set-key (kbd "C-x C-x") 'boogs/helm-mark-or-exchange-rect)
;; (global-set-key [remap apropos-command] 'helm-apropos)
;; (global-set-key [remap query-replace-regexp] 'helm-regexp)
;; (unless (boundp 'completion-in-region-function)
;;   (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point) ; TODO: Used?
;;   (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; (boogs/global-set-keys
;;  "C-x M-g" 'boogs/helm-grep-git-or-ag
;;  "C-x M-G" 'helm-do-grep-ag)

;;; Use the M-s prefix just like `occur'.
(define-key prog-mode-map (kbd "M-s f") 'helm-imenu-in-all-buffers)
;;; The text-mode-map binding targets structured text modes like Markdown.
(define-key text-mode-map (kbd "M-s f") 'helm-imenu-in-all-buffers)
(with-eval-after-load 'org
  (require 'helm-org-contacts nil t)
  (define-key org-mode-map (kbd "M-s f") 'helm-org-in-buffer-headings))
(with-eval-after-load 'woman
  (define-key woman-mode-map (kbd "M-s f") 'helm-imenu))
(with-eval-after-load 'man
  (define-key Man-mode-map (kbd "M-s f") 'helm-imenu))

(setq helm-source-names-using-follow '("Occur" "Git-Grep" "AG" "mark-ring" "Org Headings"
                                       "Imenu" "Imenu in all buffers"
                                       "All Eshell prompts" "All comint prompts"))

;;; From https://www.reddit.com/r/emacs/comments/5q922h/removing_dot_files_in_helmfindfiles_menu/.
(defun boogs/helm-skip-dots (old-func &rest args)
  "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
        (helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
        (helm-previous-line 1))))
(advice-add #'helm-preselect :around #'boogs/helm-skip-dots)
(advice-add #'helm-ff-move-to-first-real-candidate :around #'boogs/helm-skip-dots)

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'kmacro-ring)
  (add-to-list 'desktop-globals-to-save 'last-kbd-macro)
  (add-to-list 'desktop-globals-to-save 'kmacro-counter)
  (add-to-list 'desktop-globals-to-save 'kmacro-counter-format)
  (add-to-list 'desktop-globals-to-save 'helm-ff-history)
  (add-to-list 'desktop-globals-to-save 'comint-input-ring))

(helm-top-poll-mode)
;;; Column indices might need some customizing. See `helm-top-command' and
;;; https://github.com/emacs-helm/helm/issues/1586 and
;;; https://github.com/emacs-helm/helm/issues/1909.

;;; Fallback on 'find' if 'locate' is not available.
(unless (executable-find "locate")
  (setq helm-locate-recursive-dirs-command "find %s -type d -regex .*%s.*$"))

;; See https://github.com/emacs-helm/helm/issues/1962.
(defun boogs/helm-locate-meta (&optional update)
  "Like `helm-locate' but also use the databases found in /media and /run/media.
With prefix argument, UPDATE the databases with custom uptions thanks to the
'updatedb-local' script."
  (interactive "P")
  (let ((user-db (expand-file-name "~/.cache/locate.db"))
        (media-dbs (apply 'append
                          (mapcar
                           (lambda (root)
                             (append (ignore-errors
                                       (file-expand-wildcards (concat root "/*/locate.db")))
                                     ;; Also lookup subfolders; useful when root has snapshots (.e.g Btrfs).
                                     (ignore-errors
                                       (file-expand-wildcards (concat root "/*/*/locate.db")))))
                           (list (concat "/run/media/" (user-login-name))
                                 (concat "/media/" (user-login-name))
                                 "/media")))))
    (when update
      (with-temp-buffer
        (if (= (shell-command "updatedb-local" (current-buffer)) 0)
            (message "%s" (buffer-string))
          (error "%s" (current-buffer)))))
    (helm-locate-with-db
     (mapconcat 'identity
                (cons user-db media-dbs)
                ":")
     nil (thing-at-point 'filename))))

;;; Convenience.
(defun boogs/helm-toggle-visible-mark-backwards (arg)
  (interactive "p")
  (helm-toggle-visible-mark (- arg)))
(define-key helm-map (kbd "S-SPC") 'boogs/helm-toggle-visible-mark-backwards)

(defun boogs/evil-helm-set-keys ()
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line))
(add-hook 'helm-major-mode-hook 'boogs/evil-helm-set-keys)

(global-set-key  (kbd "C-<f4>") 'helm-execute-kmacro)

;; From https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm.el:
(cl-defmethod helm-setup-user-source ((source helm-source-ffiles))
  (helm-source-add-action-to-source-if
   "Magit status"
   (lambda (_candidate)
     (magit-status helm-ff-default-directory))
   source
   (lambda (candidate)
     (and (not (string-match-p ffap-url-regexp candidate))
          helm-ff-default-directory
          (locate-dominating-file helm-ff-default-directory ".git")))
   1)
  (helm-source-add-action-to-source-if
   "Add to EMMS playlist"
   (lambda (_candidate)
     (when (require 'emms nil 'noerror)
       (dolist (c (helm-marked-candidates))
         (emms-add-directory-tree c))))
   source
   (lambda (candidate)
     (or (file-directory-p candidate)
         (and (file-name-extension candidate)
              (string-match-p
               (concat (regexp-opt '("aac" "mp3" "mp4" "m4a" "ogg" "opus" "flac" "spx" "wma" "wv"))
                       "$")
                              (file-name-extension candidate)))))
   1))

(defun boogs/helm-external-command-cleanup-dotted (old-function &optional args)
  "Remove dotted programs from `helm-run-external-command' list.
Useful for Guix."
  (funcall old-function args)
  (setq helm-external-commands-list
        (cl-delete-if (lambda (p) (string-prefix-p "." p))
                      helm-external-commands-list)))
(advice-add 'helm-external-commands-list-1
            :around #'boogs/helm-external-command-cleanup-dotted)

;; From https://github.com/emacs-helm/helm/issues/2149:
(defun helm-occur-extract-urls-from-line (line)
  (with-temp-buffer
    (save-excursion (insert "\n" line))
    (cl-loop while (re-search-forward "\\(https?\\|ftp\\)://[^ >]*" nil t)
             collect (match-string 0))))

(defun helm-occur-browse-urls (_candidate)
  (let ((urls (helm-occur-extract-urls-from-line (helm-get-selection nil t))))
    (browse-url (helm-comp-read "Url: " urls :exec-when-only-one t))))

(defun helm-occur-action-transformer (actions _candidate)
  (cond ((string-match "\\(https?\\|ftp\\)://[^ >]*" (helm-get-selection nil t))
         (helm-append-at-nth actions
                             '(("Browse urls in line" . helm-occur-browse-urls))
                             1))
        (t actions)))

(cl-defmethod helm-setup-user-source ((source helm-moccur-class))
  (setf (slot-value source 'action-transformer) 'helm-occur-action-transformer))

(defun boogs/lisp-imenu-create-index ()
  (let ((index-alist '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "(\\(def\\(?:method\\|[^ ]+\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*\\(?::[^(]*\\)?\\s-*(.*)"
              nil t)
        (let ((def-type (match-string 1))
              (name (match-string 2))
              (pos (match-beginning 0)))
          (push (cons (format "%s [%s]" name def-type) pos) index-alist))))
    (nreverse index-alist)))

(defun boogs/helm-imenu ()
  "Integrate helm with imenu."
  (interactive)
  (require 'which-func)
  (let* ((imenu-auto-rescan t)
         (str (thing-at-point 'symbol))
         (init-reg (and str (concat "\\_<" (regexp-quote str) "\\_>")))
         (custom-source (helm-make-source "Custom Definitions" 'helm-imenu-source
                         :candidates (boogs/lisp-imenu-create-index))))
    (helm :sources custom-source
          :default (and str (list init-reg str))
          :preselect (helm-aif (which-function)
                         (concat "\\_<" (regexp-quote it) "\\_>")
                       init-reg)
          :buffer "*helm imenu*")))

(add-hook 'lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "M-i") #'boogs/helm-imenu)))

(defun boogs/helm-imenu-in-all-buffers ()
  "Search custom definitions across all Lisp buffers."
  (interactive)
  (require 'which-func)
  (let* ((imenu-auto-rescan t)
         (helm-highlight-matches-around-point-max-lines 'never)
         (str (thing-at-point 'symbol))
         (init-reg (and str (concat "\\_<" (regexp-quote str) "\\_>")))
         (candidates
          (cl-loop for buf in (buffer-list)
                   when (with-current-buffer buf
                          (derived-mode-p 'lisp-mode))
                   append
                   (with-current-buffer buf
                     (mapcar (lambda (item)
                              (cons (format "%s [in %s]" (car item) (buffer-name))
                                    (cons (car item) (cons buf (cdr item)))))
                            (boogs/lisp-imenu-create-index)))))
         (source (helm-make-source "Custom Definitions in All Buffers" 'helm-source-sync
                  :candidates candidates
                  :fuzzy-match t
                  :persistent-action (lambda (candidate)
                                     (switch-to-buffer (cadr candidate))
                                     (goto-char (cddr candidate))
                                     (helm-highlight-current-line))
                  :action (lambda (candidate)
                           (switch-to-buffer (cadr candidate))
                           (goto-char (cddr candidate))))))
    (helm :sources source
          :default (and str (list init-reg str))
          :preselect (helm-aif (which-function)
                        (concat "\\_<" (regexp-quote it) "\\_>")
                      init-reg)
          :buffer "*helm custom imenu*")))

(add-hook 'lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "M-s i") #'boogs/helm-imenu-in-all-buffers)))

(global-set-key (kbd "s-SPC") 'helm-rg)

(setq helm-boring-buffer-regexp-list (list
                                      (rx "*helm")
                                      (rx "*Minibuf")
                                      (rx "*Echo")
                                      (rx "*boogs/helm-sly-mini*")
                                      (rx "*string-pixel-width*")))

(require 'patch-helm)
(require 'patch-helm-comint)
(require 'patch-helm-file-name-completion)

(when (require 'helm-switch-to-repl nil :noerror)
  (helm-switch-to-repl-setup))

(provide 'init-helm)
