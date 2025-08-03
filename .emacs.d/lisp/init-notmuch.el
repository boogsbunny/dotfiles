;;--------------------------------------------------------------------
;; notmuch
;;--------------------------------------------------------------------

(require 'init-message)
(require 'patch-notmuch)
(require 'init-notmuch-sync)

(setq notmuch-fcc-dirs
      '(("boogs@venatores.group" . "vg/Sent +sent -inbox -unread")
        ("bugrahanabdulkarim@gmail.com" . "gmail/Sent +sent -inbox -unread")
        ("ba2ra@virginia.edu" . "uva/Sent +sent -inbox -unread")
        ("babdulkarim@alarm.com" . "alarm/Sent +sent -inbox -unread")))

(setq notmuch-saved-searches
      `((:name "inbox" :query "tag:inbox and date:1w.."
               :key ,(kbd "i") :sort-order newest-first)
        (:name "unread" :query "tag:unread"
               :key ,(kbd "u") :sort-order newest-first)
        (:name "flagged" :query "tag:flagged"
               :key ,(kbd "f") :sort-order newest-first)
        (:name "sent" :query "tag:sent and date:1w.."
               :key ,(kbd "t") :sort-order newest-first)
        (:name "drafts" :query "tag:draft"
               :key ,(kbd "d") :sort-order newest-first)
        (:name "work" :query "tag:work"
               :key ,(kbd "w") :sort-order newest-first)
        (:name "lists" :query "tag:mailinglist"
               :key ,(kbd "l") :sort-order newest-first)
        (:name "newsletters" :query "tag:newsletter"
               :key ,(kbd "n") :sort-order newest-first)
        (:name "all mail" :query "tag:inbox"
               :key ,(kbd "a") :sort-order newest-first)))

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

(defun boogs/notmuch-show-view-as-patch ()
  "View the the current message as a patch."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (msg (notmuch-show-get-message-properties))
         (part (notmuch-show-get-part-properties))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (notmuch-get-bodypart-text msg part nil)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
                 (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

(define-key 'notmuch-show-part-map "d" 'boogs/notmuch-show-view-as-patch)

(provide 'init-notmuch)
