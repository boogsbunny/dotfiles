;;--------------------------------------------------------------------
;; slack
;;--------------------------------------------------------------------

(require 'slack)
(require 'alert)
(require 'init-helm-slack)

(url-cookie-store "d" "")

(setq slack-buffer-emojify t
      slack-prefer-current-team t
      slack-enable-global-mode-string t
      alert-default-style 'notifier)

(slack-register-team
 :name ""
 :default t
 :cookie ""
 :token ""
 :full-and-display-names t)

(global-set-key (kbd "C-c S c") 'slack-select-rooms)
(global-set-key (kbd "C-c S u") 'slack-select-unread-rooms)
(global-set-key (kbd "C-c S m") 'slack-im-select)
(global-set-key (kbd "C-c S s") 'slack-search-from-messages)
(global-set-key (kbd "C-c S r") 'slack-thread-show-or-create)
(global-set-key (kbd "C-c S a") 'slack-message-add-reaction)
(global-set-key (kbd "C-c S e") 'slack-insert-emoji)
(global-set-key (kbd "C-c S q") 'slack-quote-and-reply)
(global-set-key (kbd "C-c S K") 'slack-stop)

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

(run-with-timer 0 (* 25 60)
                (lambda ()
                  (slack-ws--reconnect (oref slack-current-team :id) t)
                  (slack-im-list-update)))

(defun boogs/filter-slack-messages (original-logger-func message level team)
  "Filter out specific messages in slack-message-logger."
  (unless (or (string-match-p "Slack Im List Updated" message)
              (string-match-p "Reconnecting\\..." message)
              (string-match-p "[error]" message)
              (string-match-p "[info]" message))
    (funcall original-logger-func message level team)))

(advice-add 'slack-message-logger :around #'boogs/filter-slack-messages)


(provide 'init-slack)
