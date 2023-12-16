;;--------------------------------------------------------------------
;; message mode
;;--------------------------------------------------------------------

(require 'init-smtpmail)

(setq user-full-name "Boogs"
      mm-default-directory "~/Downloads"
      ;; mml-secure-openpgp-sign-with-sender t
      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it
      mail-specify-envelope-from t
      mail-envelope-from 'header)

(defun boogs/message-recipients (&optional include-from)
  "Return a list of all recipients in the message, looking at TO, CC and BCC.
Each recipient is in the format of `mail-extract-address-components'."
  (mapcan (lambda (header)
            (let ((header-value (message-fetch-field header)))
              (and
               header-value
               (mail-extract-address-components header-value t))))
          `(,@(when include-from '("From")) "To" "Cc" "Bcc")))

;; Sign messages by default.  TODO: Which method?
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
;; (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; Also crypt to self so that we can read sent e-mails.
(setq mml-secure-openpgp-encrypt-to-self t)

(defvar boogs/trust-threshold '(marignal full ultimate))
(defun boogs/trusted-addresses ()
  "Return the list of trusted email addresses in the PGP keyring.
Trust is defined as per `boogs/trust-threshold'."
  (let (valid-addresses)
    (dolist (key (epg-list-keys (epg-make-context epa-protocol)) valid-addresses)
      (dolist (user-id (epg-key-user-id-list key))
        (when (memq (epg-user-id-validity user-id) '(marginal full ultimate))
          (push (cadr (mail-extract-address-components (epg-user-id-string user-id)))
                valid-addresses))))))

(defun boogs/message-sign-encrypt-if-all-keys-trusted ()
  "Add MML tag to encrypt message when there is a key for each recipient.
Consider adding this function to `message-send-hook' to
systematically send encrypted emails when possible."
  (let* ((recipients (boogs/message-recipients))
         (untrusted-recipients (seq-difference (mapcar #'cadr recipients)
                                               (boogs/trusted-addresses))))
    (if untrusted-recipients
        (message "Not encrypting because of untrusted %s." untrusted-recipients)
      (mml-secure-message-sign-encrypt))))

(add-hook 'message-send-hook #'boogs/message-sign-encrypt-if-all-keys-trusted)

(defun boogs/cleanup-github-recipients ()
  "When replying to a github message, clean up all bogus recipients.
Also remove signature.
This function is useful in `message-setup-hook'."
  (interactive)
  (let ((to (message-fetch-field "To")))
    (when (and to
               (string-match (rx "@reply.github.com" string-end)
                             (cadr (mail-extract-address-components to))))
      (dolist (hdr '("To" "Cc" "Bcc"))
        (let ((header-value (message-fetch-field hdr)))
          (when header-value
            (message-replace-header
             hdr
             (mapconcat (lambda (addrcell)
                          (format "\"%s\" <%s>" (car addrcell) (cadr addrcell)))
                        (cl-delete-if
                         (lambda (addrcell)
                           (string-match (rx "@noreply.github.com" string-end)
                                         (cadr addrcell)))
                         (mail-extract-address-components  header-value t))
                        ", ")))))
      ;; Delete signature if any.
      (delete-region (save-excursion
                       (message-goto-signature)
                       (unless (eobp)
                         (forward-line -1))
                       (point))
                     (point-max))
      ;; Deleting trailing blank lines.
      (save-excursion
        (goto-char (point-max))
        (delete-blank-lines)
        (delete-blank-lines)))))
(add-hook 'message-setup-hook 'boogs/cleanup-github-recipients)

(defvar boogs/message-compose-fortune-p nil
  "Whether or not to include a fortune in the signature.")
(defun boogs/message-add-signature-and-maybe-fortune ()
  "Insert signature using `user-full-name'.
A fortune is appended if `boogs/message-compose-fortune-p' is non-nil."
  (require 'functions) ; For `call-process-to-string'.
  ;; Return the signature and set it for mu4e.
  (setq mu4e-compose-signature
        (concat
         user-full-name "\n"
         ;; "https://boogs.life/"
         (when (and boogs/message-compose-fortune-p
                    (executable-find "fortune"))
           (concat "\n\n"
                   (boogs/call-process-to-string "fortune" "-s"))))))
;; (add-hook 'message-setup-hook 'boogs/message-add-signature-and-maybe-fortune)
(setq message-signature 'boogs/message-add-signature-and-maybe-fortune)

(when (require 'org-contacts nil t)
  (defun boogs/message-select-dictionary ()
    "Set dictionary according to the LANGUAGE property of the first
\"To:\" recipient found in the Org contacts file."
    (interactive)
    (let ((addresses (mapcar 'cadr (boogs/message-recipients)))
          address-lang-map)
      (setq address-lang-map
            (cl-loop for contact in (org-contacts-filter)
                     ;; The contact name is always the car of the assoc-list
                     ;; returned by `org-contacts-filter'.
                     for language = (cdr (assoc-string "LANGUAGE" (nth 2 contact)))
                     ;; Build the list of the user email addresses.
                     for email-list = (org-contacts-split-property
                                       (or (cdr (assoc-string org-contacts-email-property
                                                              (nth 2 contact))) ""))
                     if (and email-list language)
                     ;; Build an alist of (EMAIL . LANGUAGE).
                     nconc (cl-loop for email in email-list
                                    collect (cons (downcase email) language))))
      (while addresses
        (if (not (assoc (car addresses) address-lang-map))
            (setq addresses (cdr addresses))
          (ispell-change-dictionary (cdr (assoc (car addresses) address-lang-map)))
          (setq addresses nil)))))
  (add-hook 'message-setup-hook 'boogs/message-select-dictionary)

  (defun boogs/message-select-sender ()
    "Set the sender according to the SENDER property of the first
\"To:\" recipient found in the Org contacts file."
    (interactive)
    (let* ((addresses (mapcar #'cadr (boogs/message-recipients)))
           (sender (cl-loop for contact in (org-contacts-filter)
                            for email-list = (org-contacts-split-property
                                              (or (cdr (assoc-string org-contacts-email-property
                                                                     (nth 2 contact))) ""))
                            when (cl-loop for email in email-list
                                          thereis (string= (downcase email)
                                                           (downcase (car addresses))))
                            return (cdr (assoc-string "SENDER" (nth 2 contact))))))
      (when sender
        (boogs/notmuch-change-sender sender))))
  (add-hook 'message-send-hook 'boogs/message-select-sender))

;; Because it's to tempting to send an e-mail riddled with typos...
(add-hook 'message-setup-hook 'flyspell-mode)

;; Org capture for emails in org-contacts
(when (require 'org-contacts nil 'noerror)
  ;; TODO: Don't duplicate contacts.
  (defun boogs/message-complete-address ()
    (require 'subr-x)
    ;; Need to get last message buffer since Org capture happens in a different
    ;; buffer.
    (let ((last-buffer
           (cl-loop for buffer in (buffer-list)
                    when (with-current-buffer buffer
                           (or (derived-mode-p 'notmuch-show-mode)
                               (derived-mode-p 'message-mode)))
                    return buffer)))
      (save-window-excursion
        (with-current-buffer last-buffer
          (let* ((recipients (boogs/message-recipients 'include-from))
                 (addresses-names (mapcar
                                   (lambda (s)
                                     (concat (cadr s) " " (car s)))
                                   recipients))
                 (email-at-point (let ((email (thing-at-point 'email)))
                                   (when email
                                     (string-trim email "<" ">"))))
                 (default (when email-at-point (seq-find (lambda (addr)
                                                           (string-prefix-p email-at-point
                                                                            addr))
                                                         addresses-names)))
                 (address-name (completing-read "Address: " addresses-names
                                                nil nil nil nil default))
                 (idx (string-match " " address-name))
                 (address (substring address-name 0 idx))
                 (name (substring address-name idx)))
            (list address name))))))

  (defun boogs/org-capture-contact-format (address name)
    (format "%s :PROPERTIES: :EMAIL: %s :END:" name address))
  (add-to-list 'org-capture-templates
               `("C" "Add e-mail address to contacts"
                 entry (file+headline ,(car org-contacts-files) "Contacts")
                 "* %(apply 'boogs/org-capture-contact-format (boogs/message-complete-address))")))

;; The following is an alternative using the template format string.  It has some missing features though:
;; - Can't use (thing-at-point 'email) as a default.
;; - Need to manually match Name and Address.
;; (add-to-list 'org-capture-templates
;;              `("c" "Add e-mail address to contacts" entry (file+headline ,(car org-contacts-files) "Contacts")
;;                "* %^{Name|%:fromname|%:to-names|%:cc-names}
;; :PROPERTIES:
;; :EMAIL: %^{Address|%:fromaddress|%:to-addresses|%:cc-addresses}
;; :END:"))

(provide 'init-message)
