;;--------------------------------------------------------------------
;; http rest client
;;--------------------------------------------------------------------

(defvar boogs/restclient-token nil)
(defun boogs/restclient-hook ()
  "Update token from a request."
  (save-excursion
    (save-match-data
      ;; update regexp to extract required data
      (when (re-search-forward "\"token\":\"\\(.*?\\)\"" nil t)
        (setq boogs/restclient-token (match-string 1))))))

(add-hook 'restclient-response-received-hook #'boogs/restclient-hook)

(provide 'init-restclient)
