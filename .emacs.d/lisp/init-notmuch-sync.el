;;--------------------------------------------------------------------
;; sync notmuch tags
;;--------------------------------------------------------------------

(require 'seq)
(require 'cl-lib)

(unless (boundp 'notmuch-command)
  (setq notmuch-command "notmuch"))

(defun notmuch-all-tags ()
  (split-string
   (with-output-to-string
     (with-current-buffer standard-output
       (call-process notmuch-command nil t
                     nil "search" "--output=tags" "--exclude=false" "*")))))

(defun notmuch-config-get-tags (query)
  (split-string
   (with-output-to-string
     (with-current-buffer standard-output
       (call-process notmuch-command nil t
                     nil "config" "get" query)))))

(defvar notmuch-unimportant-tags (append '("attachment" "draft" "encrypted"
                                           "flagged" "passed" "replied" "sent"
                                           "signed")
                                         (notmuch-config-get-tags "new.tags")
                                         (notmuch-config-get-tags "search.exclude_tags")))

(defvar notmuch-dump-file (expand-file-name "~/personal/mail/notmuch.dump"))

(defun notmuch-dump-important-tags (&optional file)
  "Dump notmuch tag database to `notmuch-dump-file'.
Messages with only `notmuch-unimportant-tags' are ignored."
  (interactive)
  (setq file (or file notmuch-dump-file))
  (let* ((important-tags (seq-difference (notmuch-all-tags) notmuch-unimportant-tags))
         (tags-arg (cons (concat "tag:" (car important-tags))
                         (cl-loop for tag in (cdr important-tags)
                                  append (list "or" (concat "tag:" tag))))))
    (apply 'call-process notmuch-command nil `(:file ,file) nil
           "dump" tags-arg)))

(provide 'init-notmuch-sync)
