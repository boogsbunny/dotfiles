;;--------------------------------------------------------------------
;; slack
;;--------------------------------------------------------------------

(require 'slack)
(require 'alert)

(setq alert-default-style 'notifier)

(setq slack-buffer-emojify t
      slack-prefer-current-team t)

(slack-register-team
 :name ""
 :default t
 :cookie ""
 :token ""
 :full-and-display-names t)
(evil-define-key 'normal slack-info-mode-map
  ",u" 'slack-room-update-messages)

(evil-define-key 'normal slack-mode-map
  ",c" 'slack-buffer-kill
  ",ra" 'slack-message-add-reaction
  ",rr" 'slack-message-remove-reaction
  ",rs" 'slack-message-show-reaction-users
  ",pl" 'slack-room-pins-list
  ",pa" 'slack-message-pins-add
  ",pr" 'slack-message-pins-remove
  ",mm" 'slack-message-write-another-buffer
  ",me" 'slack-message-edit
  ",md" 'slack-message-delete
  ",u" 'slack-room-update-messages
  ",2" 'slack-message-embed-mention
  ",3" 'slack-message-embed-channel
  "\C-n" 'slack-buffer-goto-next-message
  "\C-p" 'slack-buffer-goto-prev-message)

(evil-define-key 'normal slack-edit-message-mode-map
  ",k" 'slack-message-cancel-edit
  ",s" 'slack-message-send-from-buffer
  ",2" 'slack-message-embed-mention
  ",3" 'slack-message-embed-channel)

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
