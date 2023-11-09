;;--------------------------------------------------------------------;
;; http rest client
;;--------------------------------------------------------------------;

(define-key restclient-mode-map (kbd "M-s f") 'helm-restclient)
(add-to-list 'helm-source-names-using-follow "Sources")

(defvar my-restclient-token nil)
(defun my-restclient-hook ()
  "Update token from a request."
  (save-excursion
    (save-match-data
      ;; update regexp to extract required data
      (when (re-search-forward "\"token\":\"\\(.*?\\)\"" nil t)
        (setq my-restclient-token (match-string 1))))))

(add-hook 'restclient-response-received-hook #'my-restclient-hook)

(with-eval-after-load 'company-restclient
  (add-to-list 'company-backends 'company-restclient)
  (add-hook 'restclient-mode-hook 'company-mode)
  (define-key restclient-mode-map (kbd "M-<tab>") (if (require 'helm-company nil t)
                                                      'helm-company
                                                    'company-complete)))

(provide 'init-restclient)
