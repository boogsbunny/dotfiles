;;--------------------------------;
;; Notmuch
;;--------------------------------;

(require 'notmuch)

(setq notmuch-search-oldest-first nil
      send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
