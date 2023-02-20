;;--------------------------------;
;; TRAMP
;;--------------------------------;

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)
      tramp-inline-compress-start-size 1000
      tramp-copy-size-limit 10000
      vc-handled-backends '(Git)
      tramp-verbose 1
      remote-file-name-inhibit-cache nil
      tramp-use-ssh-controlmaster-options nil
      tramp-ssh-controlmaster-options ""
      projectile--mode-line "Projectile")

(provide 'init-tramp)
