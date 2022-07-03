;;--------------------------------;
;; Slack
;;--------------------------------;

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

(provide 'init-slack)
