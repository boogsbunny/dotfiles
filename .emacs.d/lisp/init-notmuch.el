;;--------------------------------;
;; Notmuch
;;--------------------------------;

(require 'init-message)
(require 'patch-notmuch)
(require 'init-notmuch-sync)

(setq notmuch-fcc-dirs
			'(("boogs@venatores.group" . "vg/Sent +sent -inbox -unread")
				("bugrahanabdulkarim@gmail.com" . "gmail/Sent +sent -inbox -unread")
				("ba2ra@virginia.edu" . "uva/Sent +sent -inbox -unread")
        ("babdulkarim@alarm.com" . "alarm/Sent +sent -inbox -unread")))

(setq notmuch-saved-searches
      `((:name "inbox" :query "tag:inbox and date:1w.." :key ,(kbd "i") :sort-order newest-first)
        (:name "unread" :query "tag:unread" :key ,(kbd "u") :sort-order newest-first)
        (:name "flagged" :query "tag:flagged" :key ,(kbd "f") :sort-order newest-first)
        (:name "sent" :query "tag:sent and date:1w.." :key ,(kbd "t") :sort-order newest-first)
        (:name "drafts" :query "tag:draft" :key ,(kbd "d") :sort-order newest-first)
        (:name "all mail" :query "tag:inbox" :key ,(kbd "a") :sort-order oldest-first)))

(defun boogs/notmuch-change-sender (&optional sender)
	(interactive)
	(unless (derived-mode-p 'message-mode)
		(error "Must be in message mode"))
	(unless sender
		(setq sender (completing-read "Sender: " (mapcar 'car notmuch-fcc-dirs))))
	(message-replace-header "From" sender)
	(message-remove-header "Fcc")
	(notmuch-fcc-header-setup))

(when (require 'helm-notmuch nil t)
  (setq helm-notmuch-match-incomplete-words t)
  (dolist (map (list notmuch-search-mode-map
                     notmuch-hello-mode-map
                     notmuch-show-mode-map
                     notmuch-tree-mode-map))
    (define-key map "s" 'helm-notmuch))
  (define-key notmuch-show-mode-map (kbd "M-s f") #'helm-imenu))

(defun notmuch-show-bounce (&optional address)
  "Bounce the current message."
  (interactive "sBounce To: ")
  (notmuch-show-view-raw-message)
  (message-resend address))

;; Improve address completion with Helm.
(setq notmuch-address-use-company nil)
(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))

(defun boogs/notmuch-poll-async ()
  "Like `notmuch-poll' but asynchronous."
  (notmuch-start-notmuch
   "notmuch-new"
   nil
   (lambda (_proc change)
     (with-current-buffer (cl-find-if (lambda (b)
                                        (with-current-buffer b
                                          (eq major-mode 'notmuch-search-mode)))
                                      (buffer-list))
       (notmuch-refresh-this-buffer))
     (message "notmuch-new: %s" change))
   "new"))

(advice-add 'notmuch-poll
            :override #'boogs/notmuch-poll-async)

(provide 'init-notmuch)
