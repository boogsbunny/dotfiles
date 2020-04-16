;;--------------------------------;
;; Ledger mode
;;--------------------------------;

(setq evil-ledger-sort-key "S")

(add-hook 'ledger-mode-hook #'evil-ledger-mode))

(provide 'init-ledger)
