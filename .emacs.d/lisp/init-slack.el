;;--------------------------------------------------------------------
;; slack
;;--------------------------------------------------------------------

(require 'slack)
(require 'alert)
(require 'org)
(require 'subr-x)
(require 'init-org-roam)

(defun boogs/slack-org-ensure-file ()
  "Create `boogs/org-slack-file' with agenda-friendly metadata."
  (unless (file-exists-p boogs/org-slack-file)
    (with-temp-buffer
      (insert "#+TITLE: Slack\n")
      (insert "#+CATEGORY: Slack\n\n")
      (write-region (point-min) (point-max) boogs/org-slack-file))))

(defun boogs/slack-org-clean-one-line (s &optional max)
  "Return S as a safe one-line Org heading fragment."
  (let* ((s (or s "Slack message"))
         (s (replace-regexp-in-string "[\n\r\t ]+" " " s))
         (s (string-trim s))
         (max (or max 100)))
    (if (> (length s) max)
        (concat (substring s 0 max) "...")
      s)))

(defun boogs/slack-org-alert-id (info)
  "Return a stable-enough ID for Slack alert INFO."
  (or (plist-get info :id)
      (secure-hash
       'sha1
       (format "%S"
               (list (plist-get info :title)
                     (plist-get info :message)
                     (plist-get info :category))))))

(defun boogs/slack-org-alert-exists-p (id)
  "Return non-nil if Slack alert ID already exists in the Slack org file."
  (and (file-exists-p boogs/org-slack-file)
       (with-temp-buffer
         (insert-file-contents boogs/org-slack-file)
         (goto-char (point-min))
         (search-forward (format ":SLACK_ALERT_ID: %s" id) nil t))))

(defun boogs/slack-org-capture-alert (info)
  "Append Slack alert INFO as a TODO in `boogs/org-slack-file'."
  (boogs/slack-org-ensure-file)
  (let* ((title (boogs/slack-org-clean-one-line (plist-get info :title) 80))
         (message (or (plist-get info :message) ""))
         (id (boogs/slack-org-alert-id info)))
    (unless (boogs/slack-org-alert-exists-p id)
      (with-temp-buffer
        (insert "\n")
        (insert (format "* TODO Slack: %s :slack:\n" title))
        (insert ":PROPERTIES:\n")
        (insert (format ":CREATED: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format ":SLACK_ALERT_ID: %s\n" id))
        (insert ":END:\n\n")
        (unless (string-empty-p message)
          (insert "#+begin_quote\n")
          (insert message)
          (unless (string-suffix-p "\n" message)
            (insert "\n"))
          (insert "#+end_quote\n"))
        (write-region (point-min) (point-max) boogs/org-slack-file t)))))

(alert-define-style
 'boogs/slack-org-alert-style
 :title "Add Slack alert to Org agenda"
 :notifier #'boogs/slack-org-capture-alert)

(add-to-list 'alert-user-configuration
             '(((:category . "slack")) boogs/slack-org-alert-style nil))

(setq slack-buffer-emojify t
      slack-prefer-current-team t
      slack-enable-global-mode-string t
      slack-emit-periodic-presence-p t
      alert-default-style 'message
      alert-fade-time 10)

(global-set-key (kbd "C-c s c") 'slack-select-rooms)
(global-set-key (kbd "C-c s u") 'slack-select-unread-rooms)
(global-set-key (kbd "C-c s m") 'slack-im-select)
(global-set-key (kbd "C-c s s") 'slack-search-from-messages)
(global-set-key (kbd "C-c s r") 'slack-thread-show-or-create)
(global-set-key (kbd "C-c s a") 'slack-message-add-reaction)
(global-set-key (kbd "C-c s e") 'slack-insert-emoji)
(global-set-key (kbd "C-c s q") 'slack-quote-and-reply)
(global-set-key (kbd "C-c s k") 'slack-stop)
(global-set-key (kbd "C-c s o") 'slack-start)

(define-key slack-mode-map (kbd "C-c C-o") 'slack-open-url)
(define-key slack-mode-map (kbd "@") 'slack-message-embed-mention)
(define-key slack-mode-map (kbd "#") 'slack-message-embed-channel)

(define-key slack-thread-message-buffer-mode-map
            (kbd "C-c '") 'slack-message-write-another-buffer)

(define-key slack-thread-message-buffer-mode-map
            (kbd "@") 'slack-message-embed-mention)

(define-key slack-thread-message-buffer-mode-map
            (kbd "#") 'slack-message-embed-channel)

(define-key slack-message-buffer-mode-map
            (kbd "C-c '") 'slack-message-write-another-buffer)

(define-key slack-message-compose-buffer-mode-map
            (kbd "C-c '") 'slack-message-send-from-buffer)

(defun boogs/filter-slack-messages (original-logger-func message level team)
  "Filter out noisy Slack logger messages."
  (unless (or (string-match-p "Slack Im List Updated" message)
              (string-match-p "Reconnecting\\.\\.\\." message))
    (funcall original-logger-func message level team)))

(advice-add 'slack-message-logger :around #'boogs/filter-slack-messages)

(slack-register-team
 :name "myslackteam"
 :token (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com")
 :cookie (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com^cookie")
 :full-and-display-names t
 :default t)

(provide 'init-slack)
