(defun boogs/helm-insert-file-name-completion-at-point (_candidate)
  "Insert file name completion at point.
Like `helm-insert-file-name-completion-at-point' but prompt which
of the full, abbreviated or relative paths to insert."
  (with-helm-current-buffer
    (if buffer-read-only
        (error "Error: Buffer `%s' is read-only" (buffer-name))
      (let* ((mkds        (helm-marked-candidates :with-wildcard t))
             (candidate   (car mkds))
             (end         (point))
             (tap         (helm-ffap-guesser))
             (guess       (and (stringp tap)
                               (substring-no-properties tap)))
             (beg         (helm-aif (and guess
                                         (save-excursion
                                           (when (re-search-backward
                                                  (regexp-quote guess)
                                                  (point-at-bol) t)
                                             (point))))
                              it (point)))
             (full-path-p (and (stringp guess)
                               (or (string-match-p
                                    (concat "^" (getenv "HOME"))
                                    guess)
                                   (string-match-p
                                    "\\`\\(/\\|[[:lower:][:upper:]]:/\\)"
                                    guess))))
             (escape-fn (if (memq major-mode
                                  helm-modes-using-escaped-strings)
                            #'shell-quote-argument #'identity))
             (fname-formats '(:full nil
                                    :abbreviated (4)
                                    :relative (16)
                                    :basename (64)))
             (format-fname (lambda (format)
                             (helm-ff--format-fname-to-insert
                              candidate beg end full-path-p guess
                              format)))
             (abbreviated-path (funcall format-fname (plist-get fname-formats :abbreviated)))
             (relative-path (funcall format-fname (plist-get fname-formats :relative)))
             (basename-path (funcall format-fname (plist-get fname-formats :basename)))
             (full-path (funcall format-fname (plist-get fname-formats :full)))
             (format-fname-rest (lambda (candidate format)
                                  (helm-ff--format-fname-to-insert
                                   candidate nil nil nil nil
                                   format))))
        (when (and beg end)
          (delete-region beg end))
        (let* ((path-type (completing-read "Path type to insert: "
                                           (list abbreviated-path
                                                 relative-path
                                                 basename-path
                                                 full-path)))
               (format-choice (plist-get fname-formats
                                         (cond
                                          ((string= abbreviated-path path-type) :abbreviated)
                                          ((string= relative-path path-type) :relative)
                                          ((string= basename-path path-type) :basename)
                                          ((string= full-path path-type) :full)))))
          (insert
           (funcall
            escape-fn
            path-type)
           (if (cdr mkds) " " "")
           (mapconcat escape-fn
                      (cl-loop for f in (cdr mkds)
                               collect (funcall format-fname-rest f format-choice))
                      " ")))))))

(advice-add 'helm-insert-file-name-completion-at-point
            :override 'boogs/helm-insert-file-name-completion-at-point)

(provide 'patch-helm-file-name-completion)
