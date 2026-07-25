;;; patch-pr-review.el --- Local tweaks for pr-review -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(with-eval-after-load 'pr-review-search
  (defvar-local boogs/pr-review--search-widths nil
    "Sizing tuple used by the custom PR search table renderer.")

  (defcustom boogs/pr-review-search-opened-pr-gap 3
    "Number of spaces between Opened At and PR columns."
    :type 'integer
    :group 'pr-review)

  (defcustom boogs/pr-review-search-highlight-pr-column t
    "When non-nil, apply a face to the PR column values."
    :type 'boolean
    :group 'pr-review)

  (defface boogs/pr-review-search-pr-column-face
    '((t :inherit pr-review-listview-status-face))
    "Face for PR number values in search list."
    :group 'pr-review)

  (defun boogs/pr-review--search-format-opened-at (created-at)
    "Format CREATED-AT as fixed-width local timestamp."
    (format-time-string "%Y-%m-%d %H:%M" (date-to-time created-at)))

  (defun boogs/pr-review--fit-cell (value width)
    "Fit VALUE into WIDTH using a truncation ellipsis."
    (truncate-string-to-width (or value "") width nil nil "..."))

  (defun boogs/pr-review--search-render-row (opened pr author title status)
    "Render one fixed-width search table row from OPENED..STATUS fields."
    (pcase-let ((`(,opened-width ,pr-width ,author-width ,title-width ,status-width
                                 ,opened-pr-gap ,default-gap)
                 boogs/pr-review--search-widths))
      (format (format "%%-%ds%%-%ds%%-%ds%%-%ds%%-%ds"
                      (+ opened-width opened-pr-gap)
                      (+ pr-width default-gap)
                      (+ author-width default-gap)
                      (+ title-width default-gap)
                      status-width)
              (boogs/pr-review--fit-cell opened opened-width)
              (boogs/pr-review--fit-cell pr pr-width)
              (boogs/pr-review--fit-cell author author-width)
              (boogs/pr-review--fit-cell title title-width)
              (boogs/pr-review--fit-cell status status-width))))

  (defun boogs/pr-review--search-print-entry (id cols)
    "Print one tabulated list entry with ID and COLS without align-to indent."
    (let ((beg (point))
          (inhibit-read-only t))
      (insert (boogs/pr-review--search-render-row
               (or (aref cols 0) "")
               (or (aref cols 1) "")
               (or (aref cols 2) "")
               (or (aref cols 3) "")
               (or (aref cols 4) ""))
              "\n")
      (when boogs/pr-review-search-highlight-pr-column
        (pcase-let ((`(,opened-width ,pr-width ,_author-width ,_title-width ,_status-width
                                     ,opened-pr-gap ,_default-gap)
                     boogs/pr-review--search-widths))
          (let ((pr-beg (+ beg opened-width opened-pr-gap))
                (pr-end (min (+ beg opened-width opened-pr-gap pr-width)
                             (max beg (1- (point))))))
            (when (< pr-beg pr-end)
              (add-face-text-property pr-beg pr-end
                                      'boogs/pr-review-search-pr-column-face
                                      t)))))
      (add-text-properties beg (point)
                           `(tabulated-list-id ,id tabulated-list-entry ,cols))))

  (defun boogs/pr-review--search-format-status-compact (entry)
    "Format status cell for search ENTRY without extra trailing fields."
    (let ((my-login (let-alist (pr-review--whoami-cached) .viewer.login))
          assigned
          review-requested)
      (let-alist entry
        (setq assigned
              (cl-find-if (lambda (node)
                            (equal my-login (let-alist node .login)))
                          .assignees.nodes)
              review-requested
              (cl-find-if
               (lambda (node)
                 (equal my-login
                        (let-alist node .requestedReviewer.login)))
               .reviewRequests.nodes))
        (string-join
         (delq nil
               (list
                (propertize (downcase .state)
                            'face
                            'pr-review-listview-status-face)
                (and assigned
                     (propertize "assigned"
                                 'face
                                 'pr-review-listview-status-face))
                (and review-requested
                     (propertize "review requested"
                                 'face
                                 'pr-review-listview-status-face))))
         ", "))))

  (defun boogs/pr-review--search-refresh ()
    "Refresh search buffer with explicit PR column and aligned fields."
    (unless (eq major-mode 'pr-review-search-mode)
      (user-error "Not in search buffer"))

    (let* ((opened-width 16)
           (pr-width 8)
           (author-width 18)
           (status-width 30)
           (opened-pr-gap (max 1 boogs/pr-review-search-opened-pr-gap))
           (default-gap 1)
           (gaps-width (+ opened-pr-gap (* 3 default-gap)))
           (title-width
            (max 30
                 (- (window-width)
                    (+ opened-width pr-width author-width status-width gaps-width 4)))))
      (setq-local boogs/pr-review--search-widths
                  (list opened-width pr-width author-width title-width status-width
                        opened-pr-gap default-gap)
                  tabulated-list-padding 0
                  tabulated-list-printer #'boogs/pr-review--search-print-entry
                  tabulated-list-format
                  (vector
                   (list "Opened At" opened-width nil)
                   (list "PR" pr-width nil)
                   (list "Author" author-width nil)
                   (list "Title" title-width nil)
                   (list "Status" status-width nil)))

      (let* ((all-items (pr-review--search-prs pr-review--search-query))
             (items (seq-filter
                     (lambda (item)
                       (equal (alist-get '__typename item) "PullRequest"))
                     all-items)))
        (setq-local header-line-format
                    (concat (format "Search results: %d. " (length items))
                            (unless (equal (length all-items) (length items))
                              (format "(%d non-PRs not displayed) "
                                      (- (length all-items) (length items))))
                            (propertize (format "Query: %s" pr-review--search-query)
                                        'face
                                        'font-lock-comment-face)))
        (setq-local tabulated-list-entries
                    (mapcar
                     (lambda (item)
                       (let-alist item
                         (list item
                               (vector
                                (boogs/pr-review--fit-cell
                                 (boogs/pr-review--search-format-opened-at .createdAt)
                                 opened-width)
                                (boogs/pr-review--fit-cell
                                 (format "#%s" .number)
                                 pr-width)
                                (boogs/pr-review--fit-cell
                                 (or .author.login "")
                                 author-width)
                                (boogs/pr-review--fit-cell
                                 (format "[%s] %s" .repository.nameWithOwner .title)
                                 title-width)
                                (boogs/pr-review--fit-cell
                                 (boogs/pr-review--search-format-status-compact item)
                                 status-width)))))
                     items))
        (setq-local tabulated-list--header-string
                    (boogs/pr-review--search-render-row
                     "Opened At" "PR" "Author" "Title" "Status"))
        (message "Search result refreshed, %d items." (length items)))))

  (defun boogs/pr-review-search-open-in-browser-at-point ()
    "Open the pull request URL at point in an external browser."
    (interactive)
    (unless (eq major-mode 'pr-review-search-mode)
      (user-error "Not in search buffer"))
    (if-let ((entry (get-text-property (point) 'tabulated-list-id)))
        (let ((url (alist-get 'url entry)))
          (unless (and (stringp url) (not (string-empty-p url)))
            (user-error "No pull request URL on current line"))
          (browse-url-firefox url))
      (user-error "No pull request on current line")))

  (define-key pr-review-search-mode-map (kbd "C-c C-o")
              #'boogs/pr-review-search-open-in-browser-at-point)

  (advice-remove 'pr-review--search-refresh #'boogs/pr-review--search-refresh)
  (advice-add 'pr-review--search-refresh :override #'boogs/pr-review--search-refresh))

(with-eval-after-load 'pr-review
  (defun boogs/pr-review--buffer-name (repo-owner repo-name pr-id)
    "Return expected `pr-review' buffer name for REPO-OWNER/REPO-NAME PR-ID."
    (format "*pr-review %s/%s/%s*" repo-owner repo-name pr-id))

  (defun boogs/pr-review-open-reuse-buffer (orig-fn host repo-owner repo-name pr-id
                                                    &optional new-window anchor
                                                    last-read-time)
    "Reuse existing PR review buffer instead of reloading it.
Falls back to ORIG-FN if the review buffer does not exist yet."
    (let ((existing (get-buffer (boogs/pr-review--buffer-name repo-owner repo-name pr-id))))
      (if (and (buffer-live-p existing)
               (with-current-buffer existing
                 (eq major-mode 'pr-review-mode)))
          (progn
            (funcall (if new-window
                         #'switch-to-buffer-other-window
                       #'switch-to-buffer)
                     existing)
            (when (and anchor (fboundp 'pr-review-goto-database-id))
              (with-current-buffer existing
                (pr-review-goto-database-id anchor)))
            (with-current-buffer existing
              (redisplay)
              (recenter))
            (message "PR %s/%s/%s already loaded; reusing buffer"
                     repo-owner repo-name pr-id))
        (funcall orig-fn
                 host
                 repo-owner
                 repo-name
                 pr-id
                 new-window
                 anchor
                 last-read-time))))

  (advice-remove 'pr-review-open #'boogs/pr-review-open-reuse-buffer)
  (advice-add 'pr-review-open :around #'boogs/pr-review-open-reuse-buffer))

(setq boogs/pr-review-search-highlight-pr-column t)

(provide 'patch-pr-review)
