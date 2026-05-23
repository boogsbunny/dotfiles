;;--------------------------------------------------------------------
;; pr-review
;;--------------------------------------------------------------------

(evil-ex-define-cmd "prr" #'pr-review)
(evil-ex-define-cmd "prs" #'pr-review-search)
(evil-ex-define-cmd "prn" #'pr-review-notification)

(add-to-list 'browse-url-default-handlers
             '(pr-review-url-parse . pr-review-open-url))

(provide 'init-pr-review)
