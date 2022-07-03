;;--------------------------------;
;; IRC
;;--------------------------------;

(require 'erc-image)

(setq erc-nick "boogs"
      erc-prompt-for-password nil
      erc-auto-query 'bury
      erc-join-buffer 'bury
      erc-server-reconnect-timeout 10
      erc-server-reconnect-attemps 5
      erc-image-inline-rescale 400
      erc-autojoin-channels-alist '(("irc.libera.chat" "#commonlisp" "#lispweb" "#emacs" "#guix"))
      erc-modules '(autojoin image hl-nicks))

(provide 'init-erc)
