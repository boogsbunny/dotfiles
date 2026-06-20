;;--------------------------------------------------------------------
;; consult-gh
;;--------------------------------------------------------------------

(require 'cl-lib)
(require 'subr-x)
(require 'consult-gh)

(defcustom boogs/consult-gh-privacy-mode t
  "When non-nil, avoid persisting consult-gh history in savehist."
  :type 'boolean)

(defcustom boogs/consult-gh-owner-scope-cache-ttl 900
  "Seconds to cache GitHub owner scope discovered from gh."
  :type 'integer)

(defvar boogs/consult-gh-owner-scope-cache nil
  "Cached list of owner names used for scoped searches.")

(defvar boogs/consult-gh-owner-scope-cache-time 0.0
  "Timestamp for `boogs/consult-gh-owner-scope-cache'.")

;; Important: disable broken bridge if it ever gets loaded.
(with-eval-after-load 'consult-gh-with-pr-review
  (when (fboundp 'consult-gh-with-pr-review-mode)
    (consult-gh-with-pr-review-mode -1)))

(defun boogs/browse-url-external (url &rest _args)
  "Open URL in an external browser."
  (when (and (stringp url) (not (string-empty-p url)))
    (browse-url-with-browser-kind 'external url)))

(defun boogs/consult-gh--gh-lines (&rest args)
  "Run gh with ARGS and return output lines, or nil on failure."
  (when (executable-find "gh")
    (with-temp-buffer
      (let ((status (apply #'call-process "gh" nil t nil args)))
        (when (and (integerp status) (zerop status))
          (split-string (string-trim (buffer-string)) "\n" t))))))

(defun boogs/consult-gh-owner-scope (&optional refresh)
  "Return owner scope from gh orgs + current username."
  (let* ((now (float-time))
         (stale (> (- now boogs/consult-gh-owner-scope-cache-time)
                   boogs/consult-gh-owner-scope-cache-ttl)))
    (when (or refresh stale (null boogs/consult-gh-owner-scope-cache))
      (let* ((orgs (boogs/consult-gh--gh-lines "api" "user/orgs" "--jq" ".[].login"))
             (user (car (boogs/consult-gh--gh-lines "api" "user" "--jq" ".login")))
             (owners (delete-dups (delq nil (append orgs (and user (list user)))))))
        (setq boogs/consult-gh-owner-scope-cache owners
              boogs/consult-gh-owner-scope-cache-time now)))
    boogs/consult-gh-owner-scope-cache))

(defun boogs/consult-gh-refresh-owner-scope (&rest _)
  "Refresh owner scope cache used by scoped PR searches."
  (interactive)
  (let ((owners (boogs/consult-gh-owner-scope t)))
    (message "consult-gh owner scope: %s"
             (if owners (string-join owners ", ") "empty"))))

(defun boogs/consult-gh--owner-args (&optional refresh)
  "Return repeated --owner args for consult-gh from owner scope."
  (let ((owners (boogs/consult-gh-owner-scope refresh)))
    (apply #'append
           (mapcar (lambda (owner) (list "--owner" owner)) owners))))

(defun boogs/consult-gh-search-prs-scoped (extra-args prompt &optional initial)
  "Search PRs across owner scope using EXTRA-ARGS and PROMPT."
  (let ((owners (boogs/consult-gh-owner-scope)))
    (unless owners
      (user-error "No owner scope available; run C-c g O"))
    (let ((consult-gh-args (append consult-gh-args
                                   (boogs/consult-gh--owner-args)
                                   extra-args)))
      (consult-gh-search-prs (or initial "") nil nil prompt 0))))

(defun boogs/pr-review-open-compat (host owner repo number)
  "Open PR with compatibility for old/new pr-review signatures."
  (condition-case _
      ;; New signature: (host owner repo number &optional ...)
      (pr-review-open host owner repo number)
    (wrong-number-of-arguments
     ;; Old signature fallback: (owner repo number)
     (pr-review-open owner repo number))))

(defun boogs/consult-gh-pr-review-action (cand &rest _)
  "Open consult-gh PR candidate CAND in pr-review."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (owner (consult-gh--get-username repo))
         (name (consult-gh--get-package repo))
         (num-prop (get-text-property 0 :number cand))
         (number (if (numberp num-prop)
                     num-prop
                   (string-to-number (format "%s" num-prop))))
         (host (or (and (fboundp 'consult-gh--auth-account-host)
                        (consult-gh--auth-account-host))
                   "github.com")))
    (if (fboundp 'pr-review-open)
        (boogs/pr-review-open-compat host owner name number)
      (consult-gh--pr-browse-url-action cand))))

(defun boogs/consult-gh-search-pr-review-queue (&rest _)
  "Search open PRs requesting your review across scoped owners."
  (interactive)
  (boogs/consult-gh-search-prs-scoped
   '("--review-requested" "@me" "--state" "open")
   "Review Queue PRs: "))

(defun boogs/consult-gh-search-pr-authored (&rest _)
  "Search your authored open PRs across scoped owners."
  (interactive)
  (boogs/consult-gh-search-prs-scoped
   '("--author" "@me" "--state" "open")
   "Authored PRs: "))

(defun boogs/consult-gh-search-pr-involves (&rest _)
  "Search open PRs that involve you across scoped owners."
  (interactive)
  (boogs/consult-gh-search-prs-scoped
   '("--involves" "@me" "--state" "open")
   "PRs involving me: "))

(defun boogs/consult-gh-search-pr-open (&rest _)
  "Search open PRs across scoped owners."
  (interactive)
  (boogs/consult-gh-search-prs-scoped
   '("--state" "open")
   "Open PRs: "))

(defun boogs/consult-gh-search-prs-current-repo (&rest _)
  "Search PRs for the GitHub repository of current directory."
  (interactive)
  (if-let ((repo (consult-gh--get-repo-from-directory)))
      (consult-gh-search-prs "" repo nil "Repo PRs: " 0)
    (user-error "No GitHub repository found in current directory")))

(setq consult-gh-default-clone-directory "~/repos"
      consult-gh-show-preview t
      consult-gh-preview-key "C-o"
      consult-gh-repo-action #'consult-gh--repo-browse-files-action
      consult-gh-large-file-warning-threshold 2500000
      consult-gh-confirm-before-clone t
      consult-gh-notifications-show-unread-only t
      consult-gh-prs-state-to-show "open"
      consult-gh-group-prs-by :repo
      consult-gh-group-dashboard-by :reason
      consult-gh-group-notifications-by :reason
      consult-gh-browse-url-func #'boogs/browse-url-external
      consult-gh-pr-action #'boogs/consult-gh-pr-review-action)

(when (require 'consult-gh-transient nil t)
  (setq consult-gh-default-interactive-command #'consult-gh-transient))

(when (fboundp 'consult-gh-enable-default-keybindings)
  (consult-gh-enable-default-keybindings))

(when (require 'consult-gh-embark nil t)
  (consult-gh-embark-mode 1))

(when (require 'consult-gh-forge nil t)
  (consult-gh-forge-mode 1))

(require 'consult-gh-nerd-icons nil t)

(with-eval-after-load 'savehist
  (if boogs/consult-gh-privacy-mode
      (dolist (var '(consult-gh--known-orgs-list
                     consult-gh--known-repos-list
                     consult-gh--repos-history
                     consult-gh--search-prs-history
                     consult-gh--search-issues-history
                     consult-gh--notifications-history
                     consult-gh--dashboard-history))
        (add-to-list 'savehist-ignored-variables var))
    (progn
      (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
      (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list))))

(if (fboundp 'consult-gh-transient)
    (global-set-key (kbd "C-c g t") #'consult-gh-transient)
  (global-set-key (kbd "C-c g t") #'consult-gh-search-repos))

(global-set-key (kbd "C-c g s") #'boogs/consult-gh-search-pr-review-queue)
(global-set-key (kbd "C-c g S") #'consult-gh-search-prs)
(global-set-key (kbd "C-c g a") #'boogs/consult-gh-search-pr-authored)
(global-set-key (kbd "C-c g i") #'boogs/consult-gh-search-pr-involves)
(global-set-key (kbd "C-c g o") #'boogs/consult-gh-search-pr-open)
(global-set-key (kbd "C-c g p") #'boogs/consult-gh-search-prs-current-repo)
(global-set-key (kbd "C-c g n") #'consult-gh-notifications)
(global-set-key (kbd "C-c g d") #'consult-gh-dashboard)
(global-set-key (kbd "C-c g r") #'consult-gh-search-repos)
(global-set-key (kbd "C-c g O") #'boogs/consult-gh-refresh-owner-scope)

(provide 'init-consult-gh)
