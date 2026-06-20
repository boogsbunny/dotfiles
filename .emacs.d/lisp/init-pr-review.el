;;--------------------------------------------------------------------
;; pr-review
;;--------------------------------------------------------------------

(defun boogs/pr-review-search-dispatch (&rest _)
  "Prefer fast single-select search when available."
  (interactive)
  (if (fboundp 'pr-review-search-open)
      (condition-case _
          (call-interactively #'pr-review-search-open)
        (wrong-number-of-arguments
         (call-interactively #'pr-review-search)))
    (call-interactively #'pr-review-search)))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "prr" #'pr-review)
  (evil-ex-define-cmd "prs" #'boogs/pr-review-search-dispatch)
  (evil-ex-define-cmd "prsl" #'pr-review-search)
  (evil-ex-define-cmd "prn" #'pr-review-notification))

(add-to-list 'browse-url-default-handlers
             '(pr-review-url-parse . pr-review-open-url))

;; Mark as internal so explicit external opens can bypass this handler.
(function-put 'pr-review-open-url 'browse-url-browser-kind 'internal)

(global-set-key (kbd "C-c g v") #'pr-review)
(global-set-key (kbd "C-c g s") #'pr-review-search)
(global-set-key (kbd "C-c g n") #'pr-review-notification)

(provide 'init-pr-review)
