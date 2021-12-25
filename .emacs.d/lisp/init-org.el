;; -*- lexical-binding: t; -*-

;;--------------------------------;
;; Org mode
;;--------------------------------;

(add-to-list 'org-modules 'org-habit t)

;; sane indentation
(setq org-indent-indentation-per-level 1)
(setq org-adapt-indentation nil)

(setq org-log-reschedule (quote time))

(setq org-directory "/media/personal/org")

(setq org-todo-keywords
      '((sequence
         "TODO(t@/!)"
         "NEXT(n/!)"
         "WAITING(w@/!)"
         "DELEGATED(e@/!)"
         "SOMEDAY(s/!)"
         "PROJECT(p)"
         "|"
         "DONE(d@)"
         "CANCELED(c)"
         )))

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "orange red" :weight bold))
        ("WAITING" . (:foreground "HotPink2" :weight bold))
        ))

;; Don't show trailing whitespace in calendar mode
(add-hook 'calendar-mode-hook
          (function (lambda () (setq show-trailing-whitespace nil))))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "E10450D18FFBE872")

;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.
(setq auto-save-default nil)
;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

;; Configure common tags
(setq org-tag-alist
      '((:startgroup)
                                        ; Put mutually exclusive tags here
        (:endgroup)
        ("@home" . ?H)
        ("@work" . ?W)
        ("crypt" . ?C)
        ("batch" . ?b)
        ("followup" . ?f)))

;; enable languages for source code evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (lisp . t)
   (C . t)
   (scheme . t)))

;; prevent confirmation
(setq org-confirm-babel-evaluate nil)

;; edit source block in other window
(setq org-src-window-setup 'other-window)

;; highlight and indent source code blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

;; highlight quotes
(setq org-fontify-quote-and-verse-blocks t)

;; hide leading bullets
(setq org-hide-leading-stars 't)

;; integrate Emacs diary
(setq org-agenda-include-diary t)

;; hide markup markers e.g. *bold* -> bold
(setq org-hide-emphasis-markers t)

;; maintain visible empty lines while toggling heading contents
(customize-set-variable 'org-blank-before-new-entry
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)

;;--------------------------------;
;; Org Roam
;;--------------------------------;

(setq org-roam-directory "/media/personal/org/roam"
      org-roam-dailies-directory "journal/")

(org-roam-db-autosync-mode)

(setq org-roam-completion-everywhere t)

(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)

(define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

(global-set-key (kbd "C-c n d n") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n d d") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-c n d Y") 'org-roam-dailies-capture-yesterday)
(global-set-key (kbd "C-c n d T") 'org-roam-dailies-capture-tomorrow)
(global-set-key (kbd "C-c n d y") 'org-roam-dailies-goto-yesterday)
(global-set-key (kbd "C-c n d t") 'org-roam-dailies-goto-tomorrow)
(global-set-key (kbd "C-c n d v") 'org-roam-dailies-capture-date)
(global-set-key (kbd "C-c n d c") 'org-roam-dailies-goto-date)
(global-set-key (kbd "C-c n d b") 'org-roam-dailies-goto-previous-note)
(global-set-key (kbd "C-c n d f") 'org-roam-dailies-goto-next-note)

(defun boogs/org-path (path)
  (expand-file-name path org-roam-directory))

(setq org-default-notes-file (boogs/org-path "inbox.org"))

(defvar boogs/org-roam-project-template
  '("p" "project" plain "** TODO %?"
    :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+category: ${title}\n#+filetags: project\n"
                           ("Tasks"))))

(defun boogs/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun boogs/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (boogs/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))


(defun boogs/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: project\n")
                                   :unnarrowed t))))

(defun boogs/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: project\n")
                                   :unnarrowed t))))

(defun boogs/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'boogs/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (boogs/org-roam-filter-by-tag "project"))
                     :templates (list boogs/org-roam-project-template)))

(defun boogs/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (boogs/org-roam-list-notes-by-tag "project")))

(with-eval-after-load 'org-roam
  (require 'org-roam-dailies)

  (defun boogs/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook
     'org-capture-after-finalize-hook #'boogs/org-roam-project-finalize-hook)
    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun boogs/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'boogs/org-roam-project-finalize-hook)
    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (boogs/org-roam-filter-by-tag "project")
     :templates
     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project")
        :unnarrowed t))))

  (defun boogs/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :if-new (file+head "inbox.org" "#+title: inbox\n")))))

  (defun boogs/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))
      ;; Only refile if the target file is different than the current
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))
  )

(defun boogs/get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (interactive)
  (let* ((journal-file-name
          (expand-file-name
           (format-time-string "%Y/%Y-%2m-%B.org")
           (boogs/org-path "journal/")))
         (journal-year-dir (file-name-directory journal-file-name)))
    (if (not (file-directory-p journal-year-dir))
        (make-directory journal-year-dir))
    journal-file-name))


(defun boogs/on-org-capture ()
  ;; Don't show the confirmation header text
  (setq header-line-format nil)

  ;; Control how some buffers are handled
  (let ((template (org-capture-get :key t)))
    (pcase template
      ("jj" (delete-other-windows)))))

(add-hook 'org-capture-mode-hook 'boogs/on-org-capture)

;; AGENDA
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-window-setup 'only-window
      org-agenda-span 'day
      org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t)

(setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

(setq org-agenda-custom-commands
      `(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                 (org-agenda-files '(,(boogs/org-path "inbox.org")))
                 (org-agenda-text-search-extra-files nil)))))

        ("n" "Next Tasks"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))))

(add-hook 'org-timer-set-hook #'org-clock-in)

;; CAPTURE TEMPLATES
(setq org-capture-templates
      `(("t" "Tasks")
        ("tt" "Task" entry (file ,(boogs/org-path "inbox.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("je" "General Entry" entry
         (file+olp+datetree ,(boogs/org-path "journal.org"))
         "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
        ("jt" "Task Entry" entry
         (file+olp+datetree ,(boogs/org-path "journal.org"))
         "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
        ("jj" "Journal" entry
         (file+olp+datetree ,(boogs/org-path "journal.org"))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)))

;; capture templates
;; (setq org-capture-templates
;;       '(
;;         ("t" "Tasks")
;;         ("tt" "TODO entry" entry (file+headline "inbox.org" "Tasks")
;;          "* TODO %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")
;;         ("tw" "WAITING entry" entry (file+headline "inbox.org" "Tasks")
;;          "* WAITING %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")
;;         ("te" "DELEGATED entry" entry (file+headline "inbox.org" "Tasks")
;;          "* DELEGATED %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")
;;         ("th" "HABIT entry" entry (file+headline "personal.org" "Habits")
;;          "* TODO %^{DESCRIPTION}\n SCHEDULED: <%<%Y-%m-%d %a %^{FREQUENCY}>>\n :PROPERTIES:\n :STYLE: habit\n :END:")
;;         ("tb" "BOOK RECOMMENDATION entry" entry (file+headline "inbox.org" "Books")
;;          "* SOMEDAY %^{AUTHOR} - %^{TITLE}\n :PROPERTIES:\n :CREATED: %U\n :PAGES: %^{PAGES}\n :GENRE: %^{GENRE}\n :END:\n - Recommended by: %^{recommended by}\n")
;;         ("ti" "IDEA entry" entry (file "tickler.org")
;;          "* SOMEDAY %^{TITLE}\n - %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")))


(provide 'init-org)
