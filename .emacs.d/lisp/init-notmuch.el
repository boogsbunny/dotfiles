;;--------------------------------;
;; Notmuch
;;--------------------------------;

;; (add-to-list 'load-path "/usr/bin/notmuch")
;; (require 'notmuch)

;; (use-package notmuch
;;   :ensure t)

;; (setq notmuch-search-oldest-first nil
;;       send-mail-function 'sendmail-send-it
;;       sendmail-program "/usr/bin/msmtp"
;;       mail-specify-envelope-from t
;;       message-sendmail-envelope-from 'header
;;       mail-envelope-from 'header)

(provide 'init-notmuch)
