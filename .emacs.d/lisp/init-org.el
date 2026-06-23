;; -*- lexical-binding: t; -*-
;;--------------------------------------------------------------------
;; org mode
;;--------------------------------------------------------------------

(require 'patch-org)

(add-to-list 'org-modules 'org-habit t)

(add-hook 'org-mode-hook (lambda () (setq-local fill-column 70)))

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
         "ASYNC(a@)"
         "UNFINISHED(u@)"
         "CANCELED(c@)")))

;; don't show trailing whitespace in calendar mode
(add-hook 'calendar-mode-hook
          (function (lambda () (setq show-trailing-whitespace nil))))

(require 'org-crypt)

(org-crypt-use-before-save-magic)

(setq org-tags-exclude-from-inheritance
      '("crypt"
        "project"))

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
(add-hook 'org-babel-after-execute-hook
          (lambda ()
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

(defun boogs/maybe-disable-variable-pitch ()
  ;; Disable variable-pitch in very large files to speed redisplay
  (when (> (buffer-size) 300000)
    (when (bound-and-true-p variable-pitch-mode)
      (variable-pitch-mode -1))))

(add-hook 'org-mode-hook #'boogs/maybe-disable-variable-pitch)

(defun boogs/org-large-buffer-optimizations ()
  "Reduce expensive UI features in large Org buffers."
  (when (> (buffer-size) 100000)
    (visual-line-mode -1)
    (display-line-numbers-mode 0)
    (when (bound-and-true-p variable-pitch-mode)
      (variable-pitch-mode -1))))

(add-hook 'org-mode-hook #'boogs/org-large-buffer-optimizations)

(setq org-list-indent-offset 2)

(define-key org-mode-map (kbd "C-<tab>") 'org-global-cycle)
(define-key org-mode-map (kbd "s-<tab>") 'evil-switch-to-windows-last-buffer)

(defun org-open-at-point-with-firefox ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (org-open-at-point )))

(define-key org-mode-map (kbd "C-x C-o") 'org-open-at-point-with-firefox)

;; maintain visible empty lines while toggling heading contents
(customize-set-variable 'org-blank-before-new-entry
                        '((heading . always)
                          (plain-list-item . always)))

(setq org-cycle-separator-lines 1)

(setq plstore-cache-passphrase-for-symmetric-encryption t)

;; (setq org-gcal-client-id "<client_id>"
;;       org-gcal-client-secret "<client_secret>"
;;       org-gcal-fetch-file-alist '(("<email>" .  "<path_of_org_file>")))

(require 'yasnippet)
(add-hook 'org-mode-hook #'yas-minor-mode)


(require 'init-org-roam)
(require 'init-org-agenda)
(require 'init-org-capture)
(require 'init-org-contacts)

(provide 'init-org)
