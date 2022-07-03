(defun boogs/helm-run-or-raise (exe &optional files)
  "Run asynchronously EXE or jump to the application window.
If EXE is already running just jump to his window if
`helm-raise-command' is non-nil.
When FILE argument is provided run EXE with FILE."
  (let* ((real-com (car (split-string exe)))
         (proc     (if files
                       (concat real-com " " (mapconcat #'identity files " "))
                     real-com))
         process-connection-type)
    (if (get-process proc)
        (if helm-raise-command
            (shell-command  (format helm-raise-command real-com))
          (error "Error: %s is already running" real-com))
      (when (member real-com helm-external-commands-list)
        (message "Starting %s..." real-com)
        (if files
            (start-process-shell-command
             proc nil (format "%s %s"
                              real-com
                              (mapconcat
                               (lambda (file)
                                 (shell-quote-argument
                                  (if (eq system-type 'windows-nt)
                                      (helm-w32-prepare-filename file)
                                    (expand-file-name file))))
                               files " ")))
          (start-process-shell-command proc nil real-com))
        (set-process-sentinel
         (get-process proc)
         (lambda (process event)
           (when (and (string= event "finished\n")
                      helm-raise-command
                      (not (helm-get-pid-from-process-name real-com)))
             (shell-command  (format helm-raise-command "emacs")))
           (message "%s process...Finished." process))))
      (setq helm-external-commands-list
            (cons real-com
                  (delete real-com helm-external-commands-list))))))

;; This change was rejected upstream.
(defun boogs/helm-open-file-externally (_file)
  "Open FILE with an external program.
Try to guess which program to use with
`helm-get-default-program-for-file'.
If not found or a prefix arg is given query the user which tool
to use."
  (let* ((fname      (expand-file-name (car (helm-marked-candidates))))
         (collection (helm-external-commands-list-1 'sort))
         (def-prog   (helm-get-default-program-for-file fname))
         (program    (if (or helm-current-prefix-arg (not def-prog))
                         ;; Prefix arg or no default program.
                         (prog1
                             (helm-comp-read
                              "Program: " collection
                              :must-match t
                              :name "Open file Externally"
                              :del-input nil
                              :history helm-external-command-history)
                           ;; Always prompt to set this program as default.
                           (setq def-prog nil))
                       ;; No prefix arg or default program exists.
                       def-prog)))
    (unless (or def-prog ; Association exists, no need to record it.
                ;; Don't try to record non--filenames associations (e.g urls).
                (not (file-exists-p fname)))
      (when
          (y-or-n-p
           (format
            "Do you want to make `%s' the default program for this kind of files? "
            program))
        (helm-aif (assoc (file-name-extension fname)
                         helm-external-programs-associations)
            (setq helm-external-programs-associations
                  (delete it helm-external-programs-associations)))
        (push (cons (file-name-extension fname)
                    (helm-read-string
                     "Program (Add args maybe and confirm): " program))
              helm-external-programs-associations)
        (customize-save-variable 'helm-external-programs-associations
                                 helm-external-programs-associations)))
    (boogs/helm-run-or-raise program (helm-marked-candidates))
    (setq helm-external-command-history
          (cons program
                (delete program
                        (cl-loop for i in helm-external-command-history
                                 when (executable-find i) collect i))))))

(advice-add 'helm-open-file-externally :override 'boogs/helm-open-file-externally)

(provide 'patch-helm)
