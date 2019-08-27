;;--------------------------------;
;; Org mode
;;--------------------------------;

(use-package org
  :ensure t)

;; personal journal
(use-package org-journal
  :ensure t)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; TODO states
(setq org-todo-keywords
			'((sequence
				 "TODO(t)"
				 "NEXT(n)"
				 "|"
				 "DONE(d)"
				 "CANCELED(c)"
				 )))

;; Don't show trailing whitespace in calendar mode
(add-hook 'calendar-mode-hook
					(function (lambda () (setq show-trailing-whitespace nil))))
