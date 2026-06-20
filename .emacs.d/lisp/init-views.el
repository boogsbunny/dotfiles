;;--------------------------------------------------------------------
;; Perspective-scoped window "views" via bookmark-view
;;--------------------------------------------------------------------

(require 'cl-lib)
(require 'bookmark)
(require 'subr-x)
(require 'perspective)
(require 'init-bookmark-view)

(setq bookmark-save-flag 1)

(defgroup boogs-views nil
  "Perspective-scoped view bookmarks."
  :group 'convenience)

(defcustom boogs/views-key (kbd "C-c V")
  "Keybinding for `boogs/view` (DWIM open-or-save)."
  :type 'key-sequence)

(defcustom boogs/views-prefix-key (kbd "C-c v")
  "Prefix key for view-related commands."
  :type 'key-sequence)

(defcustom boogs/views-auto-switch-perspective t
  "If non-nil, opening a view named like @{PERSP} ... switches to
PERSP."
  :type 'boolean)

(defcustom boogs/views-fallback-to-global-when-empty t
  "If non-nil, show global (unprefixed) bookmark-view views when
the current perspective has no scoped views yet."
  :type 'boolean)

(defvar boogs/view-history nil
  "Minibuffer history for `boogs/view*` commands.")

(defun boogs/view--persp-name ()
  (if (and (fboundp 'persp-curr) (persp-curr))
      (persp-name (persp-curr))
    "default"))

(defun boogs/view--prefix (&optional persp)
  "Prefix used to scope views to a perspective."
  (format "@{%s} " (or persp (boogs/view--persp-name))))

(defun boogs/view--record (bookmark-name)
  "Return full bookmark record for BOOKMARK-NAME or nil."
  (bookmark-get-bookmark bookmark-name 'noerror))

(defun boogs/view--view-bookmark-p (bookmark-name)
  "Non-nil if BOOKMARK-NAME is a bookmark-view bookmark."
  (let* ((bm (boogs/view--record bookmark-name))
         (rec (and bm (cdr bm))))
    (eq (alist-get 'handler rec) #'bookmark-view-handler)))

(defun boogs/view--all-names ()
  "All view bookmark names (in `bookmark-alist` order)."
  (bookmark-maybe-load-default-file)
  (cl-loop for (name . rec) in bookmark-alist
           when (eq (alist-get 'handler rec) #'bookmark-view-handler)
           collect name))

(defun boogs/view--scoped-names (&optional persp)
  "View bookmark names scoped to PERSP (defaults to current)."
  (let ((pfx (boogs/view--prefix persp)))
    (cl-remove-if-not (lambda (name) (string-prefix-p pfx name))
                      (boogs/view--all-names))))

(defun boogs/view--names (&optional all)
  "Return view bookmark names.
If ALL is non-nil, return all view bookmarks.
Otherwise return only scoped views, optionally falling back to global."
  (let ((scoped (boogs/view--scoped-names)))
    (cond
     (all (boogs/view--all-names))
     ((and (null scoped) boogs/views-fallback-to-global-when-empty)
      (boogs/view--all-names))
     (t scoped))))

(defun boogs/view--read (prompt &optional default all)
  "Read a view name with PROMPT.
DEFAULT is an *unprefixed* suggestion for a new name.
If ALL is non-nil, allow selecting from all views."
  (let* ((pfx (boogs/view--prefix))
         (cands (sort (boogs/view--names all) #'string<))
         (def (and default (concat pfx default)))
         (in (completing-read
              (if default
                  (format "%s (default %s): " prompt default)
                (format "%s: " prompt))
              cands nil nil nil 'boogs/view-history def)))

    ;; Important: if the user picked an EXISTING view bookmark name
    ;; (possibly unprefixed), return it as-is. Otherwise prefix it
    ;; to current perspective.

    (cond
     ((boogs/view--view-bookmark-p in) in)
     ((string-prefix-p "@{" in) in)
     (t (concat pfx (string-trim in))))))

(defun boogs/view-default-name ()
  "Default name for a new perspective-scoped view."
  (concat (boogs/view--prefix) (bookmark-view-default-name)))

;;; Core commands (scoped wrappers around bookmark-view)

;;;###autoload
(defun boogs/view (name)
  "DWIM: open view bookmark NAME if it exists, else save current view
as NAME. With prefix arg (C-u), read from all view bookmarks."
  (interactive
   (list (boogs/view--read "View" nil current-prefix-arg)))
  (bookmark-view name))

;;;###autoload
(defun boogs/view-save (name &optional no-overwrite)
  "Save current view under NAME (scoped by default).
With prefix arg (C-u), allow choosing any existing view name."
  (interactive
   (list (boogs/view--read "Save view" (bookmark-view-default-name)
                           current-prefix-arg)))
  (bookmark-view-save name no-overwrite))

;;;###autoload
(defun boogs/view-open (name)
  "Open view bookmark NAME.
With prefix arg (C-u), read from all view bookmarks."
  (interactive
   (list (boogs/view--read "Open view" nil current-prefix-arg)))
  (bookmark-view-open name))

;;;###autoload
(defun boogs/view-delete (name)
  "Delete view bookmark NAME.
With prefix arg (C-u), read from all view bookmarks."
  (interactive
   (list (boogs/view--read "Delete view" nil current-prefix-arg)))
  (bookmark-view-delete name))

;;;###autoload
(defun boogs/view-rename (old new)
  "Rename view bookmark OLD to NEW."
  (interactive
   (let* ((old (boogs/view--read "Rename view (old)" nil t))
          (new (read-string "Rename view (new): " old nil old)))
     (list old new)))
  (bookmark-view-rename old new))

(defun boogs/view--stack-regexp (&optional persp)
  "Regexp matching stack views in PERSP."
  (let ((re bookmark-view-name-regexp))
    (when (string-prefix-p "\\`" re)
      (setq re (substring re 2)))
    (concat "\\`" (regexp-quote (boogs/view--prefix persp)) re)))

;;;###autoload
(defun boogs/view-push ()
  "Push current view onto the *current perspective* view stack."
  (interactive)
  (let ((name (boogs/view-default-name)))
    (bookmark-view-save name 'no-overwrite)
    (add-to-history 'boogs/view-history name)))

;;;###autoload
(defun boogs/view-pop ()
  "Pop the most recent stacked view of the current perspective."
  (interactive)
  (let* ((re (boogs/view--stack-regexp))
         (name (or (cl-find-if (lambda (n) (string-match-p re n))
                               (boogs/view--all-names))
                   (user-error "View stack is empty for %{%s}"
                               (boogs/view--persp-name)))))
    (bookmark-view-open name)
    (bookmark-view-delete name)))

;;;###autoload
(defun boogs/view-migrate (old &optional new)
  "Rename an existing view OLD into the current perspective namespace.
If NEW is nil, default to @{CURRENT} OLD."
  (interactive
   (let* ((old (boogs/view--read "Migrate view (old)" nil t))
          (def (if (string-prefix-p "@{" old)
                   old
                 (concat (boogs/view--prefix) old)))
          (new (read-string "Migrate view (new): " def nil def)))
     (list old new)))
  (bookmark-view-rename old new))

(global-set-key boogs/views-key #'boogs/view)

(defvar boogs/views-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'boogs/view)
    (define-key map (kbd "s") #'boogs/view-save)
    (define-key map (kbd "o") #'boogs/view-open)
    (define-key map (kbd "d") #'boogs/view-delete)
    (define-key map (kbd "r") #'boogs/view-rename)
    (define-key map (kbd "p") #'boogs/view-push)
    (define-key map (kbd "P") #'boogs/view-pop)
    (define-key map (kbd "m") #'boogs/view-migrate)
    map)
  "Prefix map for view commands.")

(global-set-key boogs/views-prefix-key boogs/views-map)

;;; Auto-switch perspective when opening a scoped view
(when boogs/views-auto-switch-perspective
  (defun boogs/view--maybe-switch-persp (bm)
    (let ((name (bookmark-name-from-full-record bm)))
      (when (string-match "\\`@{\\([^}]+\\)} " name)
        (let ((persp (match-string 1 name)))
          (when (and (fboundp 'persp-switch)
                     (not (string= persp (boogs/view--persp-name))))
            (persp-switch persp))))))

  (defun boogs/bookmark-view-handler--auto-switch (orig bm)
    (boogs/view--maybe-switch-persp bm)
    (funcall orig bm))

  (advice-add 'bookmark-view-handler :around
              #'boogs/bookmark-view-handler--auto-switch))

;;; Consult integration

(defvar boogs/consult-source-views
  `(:name "Views"
          :narrow ?v
          :category bookmark
          :face consult-buffer
          :sort nil
          :items ,(lambda () (boogs/view--names))
          :action ,#'bookmark-jump
          :state ,(lambda () #'ignore))
  "Consult source listing view bookmarks (scoped, with optional
fallback).")

(defun boogs/views--install-consult-buffer-source ()
  (setq consult-buffer-sources
        (cons 'boogs/consult-source-views
              (delq
               'boogs/consult-source-views consult-buffer-sources))))

(with-eval-after-load 'consult
  (boogs/views--install-consult-buffer-source)

  ;; consult-bookmark narrowing: v = Views
  (when (boundp 'consult-bookmark-narrow)
    (setq consult-bookmark-narrow
          (cons '(?v "Views" . boogs/view--view-bookmark-p)
                (cl-remove-if (lambda (x) (eq (car x) ?v))
                              consult-bookmark-narrow)))))

(provide 'init-views)
