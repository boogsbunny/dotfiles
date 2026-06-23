;; -*- lexical-binding: t; -*-
;;--------------------------------------------------------------------
;; org capture
;;--------------------------------------------------------------------

(require 'init-org-roam)

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

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

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

(provide 'init-org-capture)
