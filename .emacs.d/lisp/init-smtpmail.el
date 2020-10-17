(setq smtpmail-smtp-server  "mail.privateemail.com"
            smtpmail-stream-type 'starttls
	          smtpmail-smtp-service 587)

;; REVIEW: If we don't set `user-mail-address', `mail-host-address' or
;; `message-user-fqdn', `message-make-fqdn' will put
;; "i-did-not-set--mail-host-address--so-tickle-me" in the In-Reply-To header.
(setq user-mail-address "boogs@venatores.group")

;; This is only useful to distinguish between similar entries in .authinfo / password-store.
(defun boogs/set-smtp-user ()
    "Set `smtpmail-smtp-user' to the value in the \"From\" field."
      (let ((header-value (message-fetch-field "From")))
	    (and
	           header-value
		        (setq smtpmail-smtp-user
			                 (substring-no-properties
					               (cadr (mail-extract-address-components header-value)))))))
(add-hook 'message-send-hook #'boogs/set-smtp-user)

(provide 'init-smtpmail)
