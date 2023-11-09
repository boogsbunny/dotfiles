;;--------------------------------------------------------------------;
;; sly-selector
;;--------------------------------------------------------------------;
(require 'patch-sly-mrepl)

(defvar boogs/sly-connection-name "sbcl-boogs")

(defun boogs/helm-sly-buffer-p (buffer)
  "Return non-nil if BUFFER has a SLY connection matching
`boogs/sly-connection-name'."
  (with-current-buffer buffer
    (and (derived-mode-p 'sly-mrepl-mode)
         sly-buffer-connection
         (sly-process sly-buffer-connection)
         (string-prefix-p boogs/sly-connection-name
                          (sly-connection-name sly-buffer-connection)))))

(defun boogs/helm-sly-buffer-non-boogs-p (buffer)
  "Return non-nil if BUFFER has a SLY connection not matching
`boogs/sly-connection-name'."
  (with-current-buffer buffer
    (and (derived-mode-p 'sly-mrepl-mode)
         sly-buffer-connection
         (sly-process sly-buffer-connection)
         (not (string-prefix-p boogs/sly-connection-name
                               (sly-connection-name sly-buffer-connection))))))

(defun boogs/helm-sly-mini-sources (name predicate)
  (list (helm-sly--c-source-connection
         (helm-sly--repl-buffer-candidates
          nil predicate)
         name)
        helm-sly-new
        (helm-sly-lisp-buffer-source)
        (helm-sly-build-buffers-source)))

(defun boogs/helm-sly-mini ()
  "Helm for Lisp connections and buffers using the
`boogs/sly-connection-name' connection."
  (interactive)
  (helm :sources (boogs/helm-sly-mini-sources
                  "Boogs's Lisp connections"
                  #'boogs/helm-sly-buffer-p)
        :buffer "*boogs/helm-sly-mini*"))

(defun boogs/helm-sly-mini-non-boogs ()
  "Helm for Lisp connections and buffers not using the
`boogs/sly-connection-name' connection."
  (interactive)
  (helm :sources (boogs/helm-sly-mini-sources
                  "Other Lisp connections"
                  #'boogs/helm-sly-buffer-non-boogs-p)
        :buffer "*boogs/helm-sly-mini*"))

(defun boogs/helm-selector-sly ()
  "Helm for `sly' buffers using the `boogs/sly-connection-name' connection."
  (interactive)
  (helm-selector
   "Boogs SLY-REPL"
   :predicate #'boogs/helm-sly-buffer-p
   :make-buffer-fn (lambda ()
                     (interactive)
                     (let ((sly-lisp-implementations
                            (list
                             (assoc (intern boogs/sly-connection-name)
                                    sly-lisp-implementations)))
                           (current-connection (car (sly--purge-connections))))
                       (if (and current-connection
                                (sly-mrepl--find-buffer current-connection)
                                (boogs/helm-sly-buffer-p
                                 (sly-mrepl--find-buffer current-connection)))
                           (call-interactively #'sly)
                         (sly))))
   :helm-sources #'boogs/helm-sly-mini))

(defun boogs/helm-selector-sly-other-window ()
  "Like `boogs/helm-selector-sly' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-sly)))

(defun boogs/helm-selector-sly-non-boogs ()
  "Helm for `sly' buffers not using the `boogs/sly-connection-name' connection."
  (interactive)
  (helm-selector
   "SLY-REPL for all but Boogs's shells."
   :predicate #'boogs/helm-sly-buffer-non-boogs-p
   :make-buffer-fn (lambda ()
                     (interactive)
                     (let ((current-connection (car (sly--purge-connections))))
                       (if (and current-connection
                                (sly-mrepl--find-buffer current-connection)
                                (boogs/helm-sly-buffer-non-boogs-p
                                 (sly-mrepl--find-buffer current-connection)))
                           ;; Make sure to call interactively so that last
                           ;; connection is reused.
                           (call-interactively #'sly)
                         (let ((current-prefix-arg '-))
                           (call-interactively #'sly)))))
   :helm-sources #'boogs/helm-sly-mini-non-boogs))

(defun boogs/helm-selector-sly-non-boogs-other-window ()
  "Like `boogs/helm-selector-sly-non-boogs' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-sly)))

(provide 'init-sly-selector)
