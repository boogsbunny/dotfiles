;;--------------------------------;
;; HTTP REST client
;;--------------------------------;

(define-key restclient-mode-map (kbd "M-s f") 'helm-restclient)
(add-to-list 'helm-source-names-using-follow "Sources")
(with-eval-after-load 'company-restclient
  (add-to-list 'company-backends 'company-restclient)
  (add-hook 'restclient-mode-hook 'company-mode)
  (define-key restclient-mode-map (kbd "M-<tab>") (if (require 'helm-company nil t)
                                                      'helm-company
                                                    'company-complete)))

(provide 'init-restclient)
