;;--------------------------------;
;; Notmuch
;;--------------------------------;

(require 'init-message)
(require 'patch-notmuch)
(require 'init-notmuch-sync)

(setq notmuch-fcc-dirs
			'(("boogs@venatores.group" . "vg/Sent +sent -inbox -unread")
				("bugrahanabdulkarim@gmail.com" . "gmail/Sent +sent -inbox -unread")
				("ba2ra@virginia.edu" . "uva/Sent +sent -inbox -unread")))

(setq notmuch-search-oldest-first nil
			notmuch-saved-searches
			`((:name "inbox" :query "tag:inbox and date:1w.." :key ,(kbd "i"))
				(:name "unread" :query "tag:unread" :key ,(kbd "u"))
				(:name "flagged" :query "tag:flagged" :key ,(kbd "f"))
				(:name "sent" :query "tag:sent and date:1w.." :key ,(kbd "t"))
				(:name "drafts" :query "tag:draft" :key ,(kbd "d"))
				(:name "all mail" :query "date:2w.." :key ,(kbd "a"))))

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

(provide 'init-notmuch)
