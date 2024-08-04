;;--------------------------------------------------------------------
;; irc
;;--------------------------------------------------------------------

(require 'erc-image)
(require 'patch-erc)
(require 'erc-desktop-notifications)

(add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")

(setq erc-nick "boogsbunny"
      erc-prompt-for-password t
      erc-auto-query 'bury
      erc-join-buffer 'bury
      erc-server-reconnect-timeout 10
      erc-server-reconnect-attemps 5
      erc-image-inline-rescale 400
      erc-autojoin-channels-alist '(("irc.libera.chat" "#commonlisp" "#lispweb" "#emacs" "#guix"))
      erc-modules '(autojoin image hl-nicks networks notifications))

(provide 'init-erc)
