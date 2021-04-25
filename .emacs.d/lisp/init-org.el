;;--------------------------------;
;; Org mode
;;--------------------------------;

(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c c") 'org-capture)

(add-to-list 'org-modules 'org-habit t)

;; sane indentation
(setq org-indent-indentation-per-level 1)
(setq org-adapt-indentation nil)

(setq org-log-into-drawer t)
(setq org-log-reschedule (quote time))

(setf (nth 4 org-emphasis-regexp-components) 10)
(load-library "org")

(setq org-directory "/media/org")

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

(setq org-agenda-files '("/media/org"))

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


;; Don't show trailing whitespace in calendar mode
(add-hook 'calendar-mode-hook
					(function (lambda () (setq show-trailing-whitespace nil))))

(setq org-journal-dir "/media/personal/journal/"
			org-journal-encrypt-journal t
			org-journal-enable-encryption t)

(require 'org-journal)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-crypt-key "E10450D18FFBE872")
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

(setq org-tag-alist '(("crypt" . ?C)))

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

(provide 'init-org)
