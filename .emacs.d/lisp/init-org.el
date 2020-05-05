;;--------------------------------;
;; Org mode
;;--------------------------------;

(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c c") 'org-capture)

(add-to-list 'org-modules 'org-habit t)

;; (org-indent-mode 1)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-journal-mode-hook 'org-indent-mode)

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

(setq org-agenda-files '("~/org"))

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
       ("ti" "IDEA entry" entry (file "tickler.org")
        "* SOMEDAY %^{TITLE}\n - %^{DESCRIPTION}\n :PROPERTIES:\n :CREATED: %U\n :END:")))


;; ;; Don't show trailing whitespace in calendar mode
;; (add-hook 'calendar-mode-hook
;; 					(function (lambda () (setq show-trailing-whitespace nil))))

;; ;; scheme requirement
;; (use-package geiser
;;   :ensure t)

;; enable languages for source code evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (lisp . t)
   (C . t)
   (scheme . t)))

;; edit source block in same windo;w
(setq org-src-window-setup 'current-window)

;; UTF-8 bullets
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; integrate Emacs diary
(setq org-agenda-include-diary t)

;; hide markup markers e.g. *bold* -> bold
(setq org-hide-emphasis-markers t)

(provide 'init-org)
