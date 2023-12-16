(require 'notmuch-message)

(defun boogs/notmuch-show-view-html+ ()
  "Open the text/html part of the current message using
`notmuch-show-view-part'."
  (interactive)
  (save-excursion
    (goto-char
     (prop-match-beginning
      (text-property-search-forward
       :notmuch-part
       "text/html"
       (lambda (value notmuch-part)
         (equal (plist-get notmuch-part :content-type)
                value)))))
    (notmuch-show-view-part)))

(provide 'patch-notmuch)
