;; -*- lexical-binding: t; -*-
;;--------------------------------------------------------------------
;; org mode
;;--------------------------------------------------------------------

(require 'patch-org)

(add-to-list 'org-modules 'org-habit t)
(add-hook 'org-mode-hook
          (function (lambda () (setq fill-column (string-to-number "80")))))

;; sane indentation
(setq org-indent-indentation-per-level 1)
(setq org-adapt-indentation nil)

(setq org-file-apps '((auto-mode . emacs)
                      (directory . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . "nyxt %s")
                      ("\\.pdf\\'" . default)))

(setq org-log-reschedule (quote time))
(setq org-duration-format (quote h:mm))
(setq org-directory org-dir)

(setq org-clock-clocked-in-display nil)

(setq org-todo-keywords
      '((sequence
         "TODO(t@/!)"
         "NEXT(n/!)"
         "WAITING(w@/!)"
         "DELEGATED(e@/!)"
         "SOMEDAY(s/!)"
         "|"
         "DONE(d@)"
         "CANCELED(c@)")))

;; don't show trailing whitespace in calendar mode
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
   (sql . t)
   (plantuml . t)
   (scheme . t)))

(setq org-babel-lisp-eval-fn #'sly-eval)

;; display images
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook (lambda ()
                                          (when org-inline-image-overlays
                                            (org-redisplay-inline-images))))

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
;; (setq org-agenda-include-diary t)

;; hide markup markers e.g. *bold* -> bold
(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook 'visual-line-mode)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(setq org-list-indent-offset 4)

(define-key org-mode-map (kbd "s-<tab>") 'org-global-cycle)

(defun org-open-at-point-with-firefox ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (org-open-at-point )))

(define-key org-mode-map (kbd "C-x C-o") 'org-open-at-point-with-firefox)

;; maintain visible empty lines while toggling heading contents
(customize-set-variable 'org-blank-before-new-entry
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)

;;--------------------------------------------------------------------;
;; org roam
;;--------------------------------------------------------------------;

(setq org-roam-directory org-roam-dir
      org-roam-dailies-directory "journal/")
(setq boogs/daily-note-filename "%<%Y-%m-%d>.org"
      boogs/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")

(org-roam-db-autosync-mode)

(setq org-roam-completion-everywhere t)

(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)

(global-set-key (kbd "C-c n I") 'org-roam-node-insert-immediate)
(global-set-key (kbd "C-c n t") 'boogs/org-roam-capture-task)

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

(setq org-agenda-files (list org-roam-dir))

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
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B"))
                             '(4))
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

;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-agenda-window-setup 'current-window
      org-agenda-span 'week
      org-agenda-start-on-weekday 1     ; Monday
      org-agenda-start-with-log-mode t

      org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c"))
      org-agenda-breadcrumbs-separator "->"
      org-agenda-todo-keyword-format "%-1s"
      org-agenda-fontify-priorities 'cookies
      org-agenda-category-icon-alist nil
      org-agenda-remove-times-when-in-prefix nil
      org-agenda-remove-timeranges-from-blocks nil
      org-agenda-compact-blocks nil
      org-agenda-block-separator ?—

      org-agenda-use-time-grid t

      org-agenda-time-grid
      '((daily today require-timed)
        (0600 0700 0800 0900 1000 1100
              1200 1300 1400 1500 1600
              1700 1800 1900 2000 2100)
        " ....." "-----------------")

      org-log-done 'time
      org-log-into-drawer t
      )

(setq org-agenda-sorting-strategy
      '(((agenda habit-down time-up priority-down category-keep)
         (todo priority-down category-keep)
         (tags priority-down category-keep)
         (search category-keep))))

(defvar org-priority-highest)

(defvar boogs/org-custom-daily-agenda
  ;; NOTE 2021-12-08: Specifying a match like the following does not
  ;; work.
  ;;
  ;; tags-todo "+PRIORITY=\"A\""
  ;;
  ;; So we match everything and then skip entries with
  ;; `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                (org-agenda-skip-function
                 `(org-agenda-skip-entry-if
                   'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "Important tasks without a date\n")))
    (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                (org-scheduled-past-days 365)
                ;; Excludes today's scheduled items
                (org-scheduled-delay-days 1)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "")
                (org-agenda-overriding-header "\nPending scheduled tasks")))
    (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                (org-scheduled-past-days 365)
                ;; Excludes today's scheduled items
                ;; (org-scheduled-delay-days 1)
                (org-agenda-block-separator nil)
                ;; (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "")
                (org-agenda-overriding-header "\nUnprocessed tasks")))
    ;; (tags-todo "*" ((org-agenda-time-grid nil)
    ;;             (org-agenda-start-on-weekday nil)
    ;;             (org-agenda-span 1)
    ;;             (org-agenda-show-all-dates nil)
    ;;             (org-scheduled-past-days 365)
    ;;             (org-agenda-block-separator nil)
    ;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
    ;;             (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
    ;;             (org-agenda-format-date "")
    ;;             (org-agenda-overriding-header "\nUnprocessed tasks")))
    (agenda "" ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "\nToday's agenda\n")))
    (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nNext three days\n")))
    (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(setq org-columns-default-format
      "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

(setq org-agenda-custom-commands
      `(
        ("A" "Daily agenda top priority tasks"
         ,boogs/org-custom-daily-agenda
         ((org-agenda-fontify-priorities nil)
          (org-agenda-dim-blocked-tasks nil)))

        ("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)
                      (org-agenda-span 1)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day (org-today))
                      (org-agenda-use-time-grid nil)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Awaiting response")
                 (org-agenda-max-todos nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                 (org-agenda-files '(,(boogs/org-path "inbox.org")))
                 (org-agenda-text-search-extra-files nil)))
          (todo "SOMEDAY"
                ((org-agenda-overriding-header "Ideas")
                 (org-agenda-files '(,(boogs/org-path "tickler.org")))
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

;; org capture templates
(setq org-capture-templates
      `(("t" "Tasks")
        ("tt" "TODO entry" entry (file ,(boogs/org-path "inbox.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("tw" "WAITING entry" entry (file ,(boogs/org-path "inbox.org"))
         "* WAITING %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:" :empty-lines 1)
        ("td" "DELEGATED entry" entry (file ,(boogs/org-path "inbox.org"))
         "* DELEGATED %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:" :empty-lines 1)
        ("tb" "BOOK RECOMMENDATION entry" entry (file ,(boogs/org-path "inbox.org"))
         "* SOMEDAY %^{AUTHOR} - %^{TITLE}\n :PROPERTIES:\n :CREATED: %U\n :PAGES: %^{PAGES}\n :GENRE: %^{GENRE}\n :END:\n - Recommended by: %^{recommended by}\n" :empty-lines 1)
        ("ti" "IDEA entry" entry (file ,(boogs/org-path "tickler.org"))
         "* SOMEDAY %^{TITLE}\n - %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:" :empty-lines 1)
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

;; org-roam capture templates for dailies
(setq org-roam-dailies-capture-template
      `(("d" "default" entry
         "journal%?"
         :if-new (file+head ,boogs/daily-note-filename
                            ,boogs/daily-note-header))
        ("t" "task" entry
         "* TODO %?\n  %U\n  %a\n  %i"
         :if-new (file+head+olp ,boogs/daily-note-filename
                                ,boogs/daily-note-header
                                ("Tasks"))
         :empty-lines 1)
        ("l" "log entry" entry
         "* %<%I:%M %p> - %?"
         :if-new (file+head+olp ,boogs/daily-note-filename
                                ,boogs/daily-note-header
                                ("Log")))
        ("j" "journal" entry
         "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
         :if-new (file+head+olp ,boogs/daily-note-filename
                                ,boogs/daily-note-header
                                ("Log")))
        ("m" "meeting" entry
         "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
         :if-new (file+head+olp ,boogs/daily-note-filename
                                ,boogs/daily-note-header
                                ("Log")))))

(defun boogs/build-contact-item (template-string contact-property)
  (if-let ((stuff (org-entry-get nil contact-property)))
      (concat (format template-string stuff) "\n")
    ""))

(defun boogs/build-phone-items (phone-string)
  (let ((phones (split-string phone-string "," t " ")))
    (mapconcat
     (lambda (phone)
       (format "TEL;CELL:%s\n" (string-trim phone)))
     phones
     "")))

(defun boogs/vcard ()
  "Create a .vcf file containing all contact information."
  (interactive)
  (write-region
   (string-join
    (org-map-entries
     (lambda ()
       (let ((id (org-entry-get nil "ID")))
         (string-join
          `("BEGIN:VCARD\nVERSION:2.1\n"
            ,(format "UID:%s\n" id)
            ,(boogs/build-contact-item "FN:%s" "ITEM")
            ,(if-let ((phones (org-entry-get nil "PHONE")))
                  (boogs/build-phone-items phones)
                "")
            ,(boogs/build-contact-item "EMAIL:%s" "EMAIL")
            ,(boogs/build-contact-item "ORG:%s" "GROUP")
            ,(boogs/build-contact-item "ADR;HOME:;;%s" "ADDRESS_HOME")
            ,(boogs/build-contact-item "ADR;WORK:;;%s" "ADDRESS_WORK")
            ,(boogs/build-contact-item "BDAY:%s" "BIRTHDAY")
            ,(format "REV:%s\n" (format-time-string "%Y-%m-%dT%T"))
            "END:VCARD")
          "")))
     "LEVEL=1")
    "\n")
   nil
   (read-file-name
    "Where to save the .vcf file?"
    "~/bunny/"
    "contacts.vcf")))

(setq plstore-cache-passphrase-for-symmetric-encryption t)

;; (setq org-gcal-client-id "<client_id>"
;;       org-gcal-client-secret "<client_secret>"
;;       org-gcal-fetch-file-alist '(("<email>" .  "<path_of_org_file>")))

(require 'yasnippet)
(add-hook 'org-mode-hook #'yas-minor-mode)


(provide 'init-org)
