;;--------------------------------------------------------------------
;; zig
;;--------------------------------------------------------------------

(add-hook 'zig-mode-hook #'tree-sitter-mode)
(add-hook 'zig-mode-hook #'tree-sitter-hl-mode)

(provide 'init-zig)
