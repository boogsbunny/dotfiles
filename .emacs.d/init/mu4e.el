;;--------------------------------;
;; Mu4e
;;--------------------------------;

;; the exact path may differ --- check it
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)

(use-package mu4e
  :config
  (remove-hook 'mu4e-main-mode-hook 'evil-collection-mu4e-update-main-view))

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
 mu4e-view-show-addresses t
 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 mu4e-hide-index-messages t
 mu4e-headers-include-related t
 )
