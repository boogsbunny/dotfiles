;; -*- lexical-binding: t; -*-
;;--------------------------------------------------------------------
;; org-roam
;;--------------------------------------------------------------------

(require 'seq)

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(setq org-roam-directory org-roam-dir
      org-roam-dailies-directory "journal/")

(setq boogs/daily-note-filename "%<%Y-%m-%d>.org"
      boogs/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")

(setq org-roam-db-gc-threshold most-positive-fixnum)
(setq org-roam-link-auto-replace nil)

(org-roam-db-autosync-mode)

(setq org-roam-completion-everywhere t)

(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n p") 'boogs/org-roam-find-project)

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
        (org-roam-capture-templates
         (list (append (car org-roam-capture-templates)
                       '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun boogs/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias
                                  (format-time-string "%Y-%B"))
                             '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: project\n")
                                   :unnarrowed t))))

(defun boogs/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias
                                  (format-time-string "%Y")) '(4))
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
  (boogs/org-refresh-agenda-files))

(with-eval-after-load 'org-roam
  (require 'org-roam-dailies)

  (defun boogs/org-roam-project-finalize-hook ()
    "Refresh `org-agenda-files' after project capture."
    (remove-hook
     'org-capture-after-finalize-hook #'boogs/org-roam-project-finalize-hook)
    (unless org-note-abort
      (boogs/org-refresh-agenda-files)))

  (defun boogs/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'boogs/org-roam-project-finalize-hook)
    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (boogs/org-roam-filter-by-tag "project")
     nil
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
        (org-refile nil nil (list "Tasks" today-file nil pos))))))

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

(provide 'init-org-roam)
