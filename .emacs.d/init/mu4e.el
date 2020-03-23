;;--------------------------------;
;; Mu4e
;;--------------------------------;

;; the exact path may differ --- check it
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
;; (eval-when-compile
;;   (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;   (require 'mu4e))

(remove-hook 'mu4e-main-mode-hook 'evil-collection-mu4e-update-main-view)

(use-package mu4e-conversation
  :ensure t)
(global-mu4e-conversation-mode)

(setq
 ;; attachments
 mu4e-attachment-dir "~/Downloads"
 mu4e-save-multiple-attachments-without-asking t
 ;; IMAP sync
 mu4e-maildir "~/.cache/mail"
 ;; sync all channels
 mu4e-get-mail-command "mbsync -a"
 mu4e-update-interval 90
 mu4e-change-filenames-when-moving t
 mu4e-context-policy nil
 ;; display
 mu4e-headers-date-format "%F %R"
 mu4e-headers-field '((:human-date .16)
                      (:flags . 6)
                      (:size . 6)
                      (:mailing-list . 10)
                      (:from . 22)
                      (:subject))
 mu4e-headers-time-format "%R"
 ;; Gmail-style-threading
 mu4e-headers-include-related t
 mu4e-view-show-addresses t
 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 mu4e-hide-index-messages t
 mu4e-headers-include-related t
 )
