;;--------------------------------------------------------------------
;; nix
;;--------------------------------------------------------------------

(require 'nix-ts-mode)
(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

(provide 'init-nix)
