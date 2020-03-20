;;--------------------------------;
;; Org mode
;;--------------------------------;

(use-package org
  :ensure t)

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

(org-indent-mode 1)

;; personal journal
(use-package org-journal
  :ensure t)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-journal-mode-hook 'org-indent-mode)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-log-into-drawer t)
(setq org-log-reschedule (quote time))

;; TODO states
(setq org-todo-keywords
      '((sequence
         "NEXT(n/!)"
         "TODO(t@/!)"
         "WAITING(w@/!)"
         "DELEGATED(e@/!)"
         "SOMEDAY(s/!)"
         "PROJECT(p)"
         "|"
         "DONE(d@)"
         "CANCELED(c)"
         )))

;; capture templates
(setq org-capture-templates
      '(
        ("t" "Tasks")
        ("tt" "TODO entry" entry (file+headline "inbox.org" "Tasks")
         "* TODO %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")
        ("tw" "WAITING entry" entry (file+headline "inbox.org" "Tasks")
         "* WAITING %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")
        ("te" "DELEGATED entry" entry (file+headline "inbox.org" "Tasks")
         "* DELEGATED %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")
        ("th" "HABIT entry" entry (file+headline "personal.org" "Habits")
         "* TODO %^{DESCRIPTION}\n SCHEDULED: <%<%Y-%m-%d %a %^{FREQUENCY}>>\n :PROPERTIES:\n :STYLE: habit\n :END:")
        ("tb" "BOOK RECOMMENDATION entry" entry (file+headline "inbox.org" "Books")
         "* SOMEDAY %^{AUTHOR} - %^{TITLE}\n :PROPERTIES:\n :CREATED: %U\n :PAGES: %^{PAGES}\n :GENRE: %^{GENRE}\n :END:\n - Recommended by: %^{recommended by}\n")
        ("ti" "IDEA entry" entry (file+headline "inbox.org" "Tasks")
         "* SOMEDAY %^{TITLE}\n - %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")))

(setq org-agenda-files '("~/org"))

;; Don't show trailing whitespace in calendar mode
(add-hook 'calendar-mode-hook
					(function (lambda () (setq show-trailing-whitespace nil))))

;; enable languages for source code evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (python .t)
   (lisp .t)
   (C .t)))

;; UTF-8 bullets
;; legacy package
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; unofficial successor
(use-package org-superstar
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; integrate Emacs diary
(setq org-agenda-include-diary t)

;; hide markup markers e.g. *bold* -> bold
(setq org-hide-emphasis-markers t)
