;;--------------------------------;
;; SQL
;;--------------------------------;

;; remove default login parameters
(setq sql-postgres-login-params nil)

(defun my-pass (key)
  (string-trim-right
   (shell-command-to-string (concat "pass " key))))

;; define connections
(setq sql-connection-alist
      '((localhost (sql-product 'postgres) (sql-database (concat "postgresql://"
                                                                 "postgres"
                                                                 ":password"
                                                                 "@localhost"
                                                                 ":5432"
                                                                 "/")))))

(provide 'init-sql)
