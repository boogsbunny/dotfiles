;;--------------------------------;
;; EWW
;;--------------------------------;

(require 'helm-eww nil t)

(setq eww-bookmarks-directory (expand-file-name "/media/personal/bookmarks"))
(defvar boogs/eww-bookmarks-file (expand-file-name "eww-bookmarks.gpg" eww-bookmarks-directory))

(provide 'init-eww)
