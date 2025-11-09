;;--------------------------------------------------------------------
;; consult-tramp
;;--------------------------------------------------------------------

(require 'subr-x)
(eval-when-compile (require 'cl-lib))

(defgroup consult-tramp nil
  "TRAMP picker for Vertico/Consult."
  :group 'tramp
  :prefix "consult-tramp-")

(defcustom consult-tramp-default-method "ssh"
  "Default TRAMP method for multi-hop entries."
  :type 'string)

(defcustom consult-tramp-docker-user nil
  "Login username(s) to offer for docker TRAMP.
Nil means do not add user-specific docker entries.
A string adds one user. A list of strings adds multiple users."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Single user")
                 (repeat :tag "Multiple users" string)))

(defcustom consult-tramp-localhost-directory "/"
  "Initial directory for /sudo:root@localhost:."
  :type 'string)

(defcustom consult-tramp-control-master nil
  "If non-nil, offer candidates from SSH ControlMaster sockets."
  :type 'boolean)

(defcustom consult-tramp-control-master-path "~/.ssh/"
  "Directory where SSH ControlMaster sockets are stored."
  :type 'directory)

(defcustom consult-tramp-control-master-prefix "master-"
  "Prefix of SSH ControlMaster socket files."
  :type 'string)

(defcustom consult-tramp-custom-connections '()
  "Manual extra TRAMP connections to include.
Example: '(/ssh:example:/ /ssh:host|sudo:root@host:/)."
  :type '(repeat string))

(defcustom consult-tramp-ssh-config "~/.ssh/config"
  "Path to your SSH config."
  :type 'file)

(defcustom consult-tramp-pre-command-hook nil
  "Hook run before prompting.
Called with one non-nil argument."
  :type 'hook)

(defcustom consult-tramp-post-command-hook nil
  "Hook run after opening target.
Called with one non-nil argument."
  :type 'hook)

(defcustom consult-tramp-quit-hook nil
  "Hook run when `consult-tramp-quit' is called.
Called with one non-nil argument."
  :type 'hook)

(defun consult-tramp-quit ()
  "Quit consult-tramp and kill all remote buffers."
  (interactive)
  (run-hooks 'consult-tramp-quit-hook)
  (tramp-cleanup-all-buffers))

(defun consult-tramp--directory-files (dir regexp)
  "Return list of files in DIR whose names match REGEXP."
  (let ((files nil))
    (dolist (f (sort (file-name-all-completions "" dir) #'string<))
      (unless (member f '("./" "../"))
        (let ((abs (expand-file-name f dir)))
          (when (and (not (file-directory-p abs))
                     (string-match regexp (file-name-nondirectory abs)))
            (push abs files)))))
    (nreverse files)))

(defun consult-tramp--ssh-config-includes (line)
  "Return list of files for an Include LINE from ssh config."
  (when (string-match "^\\s-*Include\\s-+\\(.+\\)$" line)
    (let* ((arg (string-trim (match-string 1 line)))
           (abs (if (file-name-absolute-p arg)
                    arg
                  (expand-file-name arg (file-name-as-directory "~/.ssh")))))
      (or (file-expand-wildcards abs t) (and (file-exists-p abs) (list abs))))))

(defun consult-tramp--ssh-hosts (&optional file seen)
  "Collect TRAMP ssh-style candidates from SSH config FILE.
SEEN is the set of already visited files to avoid recursion loops."
  (let* ((file (or file consult-tramp-ssh-config))
         (seen (cons (expand-file-name file) (or seen '())))
         (hosts '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (trim (string-trim (car (split-string line "#")))))
            (cond
             ;; Host entries
             ((string-match "^\\s-*Host\\s-+\\(.+\\)$" trim)
              (let* ((spec (match-string 1 trim))
                     (names (split-string spec "[ \t]+" t)))
                (dolist (name names)
                  (unless (string= name "*")
                    (push (format "/%s:%s:" tramp-default-method name)
                          hosts)
                    (push (format "/%s:%s|sudo:root@%s:/"
                                  consult-tramp-default-method
                                  name name)
                          hosts)))))
             ;; Include directives
             ((string-match "^\\s-*Include\\s-+" trim)
              (dolist (inc (or (consult-tramp--ssh-config-includes trim) '()))
                (let ((abs (expand-file-name inc)))
                  (unless (member abs seen)
                    (setq hosts (nconc (consult-tramp--ssh-hosts abs seen)
                                       hosts))))))))
          (forward-line 1))))
    (nreverse hosts)))

(defun consult-tramp--controlmaster-cands ()
  "Candidates derived from SSH ControlMaster sockets."
  (when consult-tramp-control-master
    (let ((dir (expand-file-name consult-tramp-control-master-path))
          (rx consult-tramp-control-master-prefix)
          (out '()))
      (dolist (f (ignore-errors
                   (consult-tramp--directory-files dir rx)))
        (let ((base (file-name-nondirectory f)))
          (when (string-match
                 (concat (regexp-quote consult-tramp-control-master-prefix)
                         "\\([^@]+\\)@\\([^:]+\\):\\([0-9]+\\)$")
                 base)
            (let ((user (match-string 1 base))
                  (host (match-string 2 base))
                  (port (match-string 3 base)))
              (push (format "/%s:%s@%s#%s:"
                            tramp-default-method user host port)
                    out)
              (push (format "/%s:%s@%s#%s|sudo:root@%s:/"
                            consult-tramp-default-method
                            user host port host)
                    out))))))
      (nreverse out)))

(defun consult-tramp--docker-cands ()
  "Docker candidates using TRAMP docker method."
  (when (and (executable-find "docker")
             (or (version<= "29.0.60" emacs-version)
                 (require 'docker-tramp nil t)))
    (let ((names (ignore-errors
                   (apply #'process-lines
                          "docker" '("ps" "--format" "{{.Names}}"))))
          (out '()))
      (dolist (name names)
        (when (and name (not (string-empty-p name)))
          (push (format "/docker:%s:/" name) out)
          (cond
           ((stringp consult-tramp-docker-user)
            (push (format "/docker:%s@%s:/" consult-tramp-docker-user name)
                  out))
           ((listp consult-tramp-docker-user)
            (dolist (u consult-tramp-docker-user)
              (push (format "/docker:%s@%s:/" u name) out))))))
      (nreverse out))))

(defun consult-tramp--vagrant-cands ()
  "Vagrant candidates if vagrant-tramp is available."
  (when (require 'vagrant-tramp nil t)
    (let ((boxes (ignore-errors
                   (cl-map 'list #'cadr (vagrant-tramp--completions)))))
      (cl-loop for box in boxes
               append (list (format "/vagrant:%s:/" box)
                            (format "/vagrant:%s|sudo:%s:/" box box))))))

(defun consult-tramp--all-cands ()
  "Return a list of TRAMP target strings."
  (let* ((ssh (consult-tramp--ssh-hosts))
         (ctl (consult-tramp--controlmaster-cands))
         (doc (consult-tramp--docker-cands))
         (vag (consult-tramp--vagrant-cands))
         (sudo-local (list (concat "/sudo:root@localhost:"
                                   consult-tramp-localhost-directory)))
         (all (append consult-tramp-custom-connections ssh ctl doc vag
                      sudo-local)))
    ;; Deduplicate while preserving order.
    (cl-remove-duplicates all :test #'string=)))

(defun consult-tramp--annotate (cand)
  "Annotation function for CAND."
  (cond
   ((string-prefix-p "/ssh:" cand) " ssh")
   ((string-prefix-p "/docker:" cand) " docker")
   ((string-prefix-p "/vagrant:" cand) " vagrant")
   ((string-prefix-p "/sudo:" cand) " sudo")
   (t "")))

(defun consult-tramp--read (prompt)
  "Read a TRAMP candidate with PROMPT.
Uses Consult's consult--read when available, otherwise completing-read."
  (let* ((cands (consult-tramp--all-cands)))
    (cond
     ((and (fboundp 'consult--read) (fboundp 'consult--lookup-cdr))
      ;; Use Consult's enhanced reader with alist mapping.
      (let* ((alist (mapcar (lambda (s) (cons s s)) cands)))
        (consult--read alist
                       :prompt prompt
                       :require-match t
                       :category 'file
                       :lookup #'consult--lookup-cdr
                       :history 'consult-tramp--history)))
     (t
      ;; Plain completing-read (Vertico handles UI if enabled).
      (let ((completion-extra-properties
             '(:annotation-function consult-tramp--annotate)))
        (completing-read prompt cands nil t nil
                         'consult-tramp--history))))))

(defun consult-tramp--open (path)
  "Open PATH via TRAMP with `find-file'."
  (find-file path))

(defun consult-tramp--open-shell (path)
  "Open an interactive shell with PATH as default-directory."
  (let ((default-directory path))
    (shell (concat "* consult-tramp shell - " path " *"))))

;;;###autoload
(defun boogs/consult-tramp (&optional shell)
  "Pick a TRAMP target and open it with `find-file'.
With prefix argument SHELL (C-u), open a shell instead."
  (interactive "P")
  (when (and (not (file-exists-p consult-tramp-ssh-config))
             (null consult-tramp-custom-connections))
    (message "Warning: %s does not exist; only non-SSH sources will appear."
             consult-tramp-ssh-config))
  (run-hooks 'consult-tramp-pre-command-hook)
  (let ((target (consult-tramp--read "TRAMP target: ")))
    (when (and (stringp target) (not (string-empty-p target)))
      (if shell
          (consult-tramp--open-shell target)
        (consult-tramp--open target))
      (run-hooks 'consult-tramp-post-command-hook))))

;;;###autoload
(defun boogs/consult-tramp-shell ()
  "Pick a TRAMP target and open an interactive shell at that location."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'consult-tramp)))

(provide 'init-consult-tramp)
