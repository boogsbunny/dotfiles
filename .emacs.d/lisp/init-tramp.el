;;--------------------------------------------------------------------
;; tramp
;;--------------------------------------------------------------------

(setq projectile--mode-line "Projectile"

      remote-file-name-inhibit-auto-save-visited t
      remote-file-name-inhibit-cache nil
      remote-file-name-inhibit-locks t

      tramp-copy-size-limit 10000
      tramp-inline-compress-start-size 1000
      tramp-use-scp-direct-remote-copying t
      tramp-use-ssh-controlmaster-options nil
      tramp-ssh-controlmaster-options ""
      tramp-verbose 1

      vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)
      vc-handled-backends '(Git)
      )

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'compile
  (remove-hook 'compilation-mode-hook
               #'tramp-compile-disable-ssh-controlmaster-options))

(remove-hook 'find-file-hook 'forge-bug-reference-setup)

(require 'init-consult-tramp)

(provide 'init-tramp)
