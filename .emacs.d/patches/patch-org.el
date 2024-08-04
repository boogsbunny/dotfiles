(setq org-modern-label-border 1
      org-modern-variable-pitch nil
      org-modern-timestamp t
      org-modern-table t
      org-modern-table-vertical 1
      org-modern-table-horizontal 0
      org-modern-list '((?+ . "•")
                        (?- . "–")
                        (?* . "◦"))
      org-modern-internal-target nil
      org-modern-radio-target nil)

(global-org-modern-mode)

(setq line-spacing 0.1
      org-auto-align-tags nil
      org-tags-colum 0
      org-catch-invisible-edits 'show-and-error
      org-insert-heading-respect-content t

      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis "...")

(require 'org-alert)
(setq alert-default-style 'libnotify)

(setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)

(setq org-alert-time-match-string
      "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\).*>")

;; org modern
;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "Fira Mono" :height 100))))
;;  '(fixed-pitch ((t ( :family "Fira Code Retina" :height 100)))))

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; svg tags
(require 'svg-tag-mode)
(setq svg-tag-tags
      '((":TODO:" . ((lambda (tag) (svg-tag-make "TODO"))))
        (":DONE:" . ((lambda (tag) (svg-tag-make "DONE"))))))

(setq svg-tag-tags
      '(("\\(:[A-Z]+\\)\|[a-zA-Z#0-9]+:" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :inverse t
                                                          :margin 0 :crop-right t))))
        (":[A-Z]+\\(\|[a-zA-Z#0-9]+:\\)" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :end -1
                                                         :margin 0 :crop-left t))))))

(setq svg-tag-tags
      '(("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                     (svg-tag-make tag :beg 2))))
        ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2 :end -1))))))

(defun org-agenda-show-svg ()
  (let* ((case-fold-search nil)
         (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
         (keyword (car keywords)))
    (while keyword
      (save-excursion
        (while (re-search-forward (nth 0 keyword) nil t)
          (overlay-put (make-overlay
                        (match-beginning 0) (match-end 0))
                       'display  (nth 3 (eval (nth 2 keyword)))) ))
      (pop keywords)
      (setq keyword (car keywords)))))
(add-hook 'org-agenda-finalize-hook #'org-agenda-show-svg)

(svg-tag-mode t)

(defvar org-dir
  (if (file-directory-p "/media/personal/org")
      "/media/personal/org"
    "~/org"))

(defvar org-roam-dir
  (if (file-directory-p "/media/personal/org/roam")
      "/media/personal/org/roam"
    "~/org/roam"))

(provide 'patch-org)
