;;--------------------------------------------------------------------
;; sql
;;--------------------------------------------------------------------

;; remove default login parameters
(setq sql-postgres-login-params nil)

;; evil-collection binds RET to newline in insert state for comint
;; REPLs by default; force SQLi insert RET to submit input.
(with-eval-after-load 'sql
  (with-eval-after-load 'evil
    (evil-define-key 'insert sql-interactive-mode-map
      (kbd "RET") #'comint-send-input
      (kbd "C-m") #'comint-send-input
      [return] #'comint-send-input)))

;; define connections
(setq sql-connection-alist
      '((localhost (sql-product 'postgres) (sql-database (concat "postgresql://"
                                                                 "postgres"
                                                                 ":password"
                                                                 "@localhost"
                                                                 ":5432"
                                                                 "/")))))

(provide 'init-sql)
