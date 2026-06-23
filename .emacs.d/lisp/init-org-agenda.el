;; -*- lexical-binding: t; -*-
;;--------------------------------------------------------------------
;; org agenda
;;--------------------------------------------------------------------

(require 'seq)
(require 'init-org-roam)

(defvar org-priority-highest)

(defvar boogs/org-agenda-recent-days 7)
(defvar boogs/org-agenda-recent-dailies-count 3)

(defun boogs/org-existing-files (&rest files)
  (seq-filter #'file-exists-p files))

(defun boogs/org-dailies-directory ()
  (expand-file-name org-roam-dailies-directory org-roam-directory))

(defun boogs/org-recent-daily-files (&optional days)
  (let ((days (or days boogs/org-agenda-recent-days))
        files)
    (dotimes (n days)
      (let* ((time (time-subtract (current-time) (days-to-time n)))
             (file (expand-file-name
                    (format-time-string boogs/daily-note-filename time)
                    (boogs/org-dailies-directory))))
        (push file files)))
    (seq-filter #'file-exists-p (nreverse files))))

(defun boogs/org-last-existing-daily-files (&optional count)
  (let* ((count (or count boogs/org-agenda-recent-dailies-count))
         (dir (boogs/org-dailies-directory)))
    (when (file-directory-p dir)
      (seq-take
       (reverse
        (sort
         (directory-files dir t "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\'")
         #'string<))
       count))))

(defun boogs/org-agenda-project-files ()
  (boogs/org-roam-list-notes-by-tag "project"))

(defun boogs/org-refresh-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (delete-dups
         (append
          (boogs/org-existing-files
           (boogs/org-path "inbox.org")
           (boogs/org-path "tickler.org"))
          (boogs/org-agenda-project-files)
          (boogs/org-recent-daily-files)
          (boogs/org-last-existing-daily-files)))))

(boogs/org-refresh-agenda-files)

;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-window-setup 'current-window
      org-agenda-span 'week
      org-agenda-start-on-weekday 1     ; Monday
      org-agenda-start-with-log-mode nil

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
      org-clock-out-remove-zero-time-clocks t)

(setq org-agenda-sorting-strategy
      '(((agenda habit-down time-up priority-down category-keep)
         (todo priority-down category-keep)
         (tags priority-down category-keep)
         (search category-keep))))

(defvar boogs/org-day-agenda
  '((agenda "" ((org-deadline-warning-days 0)
                (org-agenda-span 1)
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day (org-today))
                (org-agenda-overriding-header "Today\n")))
    (tags-todo "+PRIORITY=\"A\""
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                (org-agenda-overriding-header "\nFocus\n")))
    (tags-todo "+TODO=\"TODO\"-PRIORITY=\"A\"-followup"
               ((org-agenda-files (boogs/org-last-existing-daily-files))
                (org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                (org-agenda-overriding-header "\nRecent daily tasks\n")))
    (tags-todo "+TODO=\"TODO\"-PRIORITY=\"A\"-followup"
               ((org-agenda-files `(,(boogs/org-path "inbox.org")))
                (org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                (org-agenda-overriding-header "\nInbox\n"))))
  "Focused agenda for today's work, avoiding duplicated task sections.")

(defvar boogs/org-review-agenda
  '((agenda "" ((org-deadline-warning-days 14)
                (org-agenda-span 14)
                (org-agenda-start-on-weekday nil)
                (org-agenda-overriding-header "Next 14 days\n")))
    (tags-todo "+followup"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                (org-agenda-overriding-header "\nNeeds follow up\n")))
    (tags-todo "+TODO=\"NEXT\"-followup"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                (org-agenda-overriding-header "\nNext actions\n")))
    (tags-todo "+TODO=\"WAITING\"-followup"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                (org-agenda-overriding-header "\nAwaiting response\n")))
    (tags-todo "+TODO=\"SOMEDAY\"-followup"
               ((org-agenda-files `(,(boogs/org-path "tickler.org")))
                (org-agenda-overriding-header "\nIdeas\n"))))
  "Review agenda for triage and next-action planning.")

(setq org-columns-default-format
      "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

(setq org-agenda-custom-commands
      `(("d" "Day agenda"
         ,boogs/org-day-agenda
         ((org-agenda-fontify-priorities nil)
          (org-agenda-dim-blocked-tasks nil)))
        ("r" "Review"
         ,boogs/org-review-agenda
         ((org-agenda-fontify-priorities nil)
          (org-agenda-dim-blocked-tasks nil)))))

(add-hook 'org-clock-in-hook 'org-timer-start)
(add-hook 'org-clock-out-hook 'org-timer-stop)

(provide 'init-org-agenda)
