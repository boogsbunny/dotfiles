;;--------------------------------------------------------------------
;; guix
;;--------------------------------------------------------------------

(defun boogs/init-guix ()
  (and buffer-file-name
       (string-match "\\<guix\\>" buffer-file-name)
       (guix-devel-mode)))
(add-hook 'scheme-mode-hook 'boogs/init-guix)

(defun boogs/guix-debbugs-gnu (&optional severities packages archivedp suppress tags)
  "Like `debbugs-gnu' but for the Guix project."
  (interactive)
  (let ((debbugs-gnu-default-packages '("guix-patches" "guix")))
    (if (called-interactively-p)
        (call-interactively 'debbugs-gnu)
      (debbugs-gnu severities packages archivedp suppress tags))))

(require 'guix nil 'noerror)

(defun boogs/guix-generations-list-diff-this ()
  "List guix-generation-list-diff but compare generation at point
with previous."
  (interactive)
  (let ((diff-fun #'guix-diff)
        (gen-fun #'guix-profile-generation-packages-buffer))
    (funcall diff-fun
             (funcall gen-fun (1- (bui-list-current-id)))
             (funcall gen-fun (bui-list-current-id)))))

(with-eval-after-load 'guix-ui-generation
  (define-key guix-generation-list-mode-map "=" #'boogs/guix-generations-list-diff-this))

(defvar boogs/guix-extra-channels "~/.guix-extra-channels")
(defvar boogs/guix-extra-profiles "~/.guix-extra-profiles")
(defvar boogs/guix-manifest-directory "~/.package-lists")
(defvar boogs/guix-system-directory "~/.config/guix/system")
(defvar boogs/guix-channel-spec-directory "~/.package-lists")
(defvar boogs/guix-always-use-channel-specs nil
  "If non-nil, automatically use a channel specification matching the chosen manifest.
The channel specification is looked up in
`boogs/guix-channel-spec-directory'.")

(cl-defun boogs/guix-query-file (&key file directory
                                         (filter ".")
                                         (prompt "File: ")
                                         (name-function #'identity)
                                         (multiple? nil))
  "Query a file matching FILTER in DIRECTORY.
Return (NAME FILE).
If FILE is non-nil, then this function is useful to derive the name of the manifest.
NAME-FUNCTION takes the file base name as argument and returns NAME.
If MULTIPLE? is non-nil, return a list of (NAME FILE) of the selected manifests."
  (cl-flet ((name (file)
                  (replace-regexp-in-string
                   "guix-" ""
                   (funcall name-function
                            (file-name-base file)))))
    (if file
        (list (name file) file)
      (let ((files (mapcar (lambda (file)
                             (list (name file) file))
                           (directory-files directory 'full filter))))
        (if multiple?
            (mapcar (lambda (name)
                      (assoc name files))
                    (completing-read-multiple prompt (mapcar #'cl-first files)))
          (assoc (completing-read prompt (mapcar #'cl-first files))
                 files))))))

(defun boogs/guix-query-manifest (&optional manifest multiple?)
  "Query a manifest as found in `boogs/guix-manifest-directory'.
Return (NAME FILE).
If MANIFEST is non-nil, then this function is useful to derive the name of the manifest.
If MULTIPLE? is non-nil, allow querying multiple manifests."
  (boogs/guix-query-file
   :file manifest
   :directory boogs/guix-manifest-directory
   :filter "manifest"
   :prompt "Manifest(s): "
   :name-function (lambda (name)
                    (replace-regexp-in-string "-?manifest-?" "" name))
   :multiple? multiple?))

(defun boogs/guix-query-system (&optional system)
  "Query a system as found in `boogs/guix-system-directory'.
Return (NAME FILE).
If SYSTEM is non-nil, then this function is useful to derive the name of the system. "
  (boogs/guix-query-file
   :file system
   :directory boogs/guix-system-directory
   :filter "scm"
   :prompt "System: "))

(defun boogs/guix-query-channel-spec (&optional channel-spec)
  "Query a channel specification as found in `boogs/guix-channel-spec-directory'.
Return (NAME FILE).
If CHANNEL-SPEC is non-nil, then this function is useful to derive the name of
the channel specification."
  (boogs/guix-query-file
   :file channel-spec
   :directory boogs/guix-channel-spec-directory
   :filter "channels"
   :prompt "Channel specification: "
   :name-function (lambda (name)
                    (replace-regexp-in-string "-?channels?-?" "" name))))

(defun boogs/guix-edit-system (&optional system)
  "Edit system.
If SYSTEM is nil, it is queried from the systems found in `boogs/guix-system-directory'."
  (interactive)
  (setq system (cl-second (boogs/guix-query-system system)))
  (find-file system))
(global-set-key (kbd "C-x c s") #'boogs/guix-edit-system)
(with-eval-after-load 'evil
  ;; For some reason `global-set-key' does not work for Evil at this point.
  (dolist (mode '(normal insert))
    (evil-global-set-key mode (kbd "C-x c s") #'boogs/guix-edit-system)))

(defun boogs/guix-edit-manifest (&optional manifest)
  "Edit MANIFEST.
If MANIFEST is nil, it is queried from the manifests found in `boogs/guix-manifest-directory'."
  (interactive)
  (setq manifest (cl-second (boogs/guix-query-manifest manifest)))
  (find-file manifest))
(global-set-key (kbd "C-x c g") #'boogs/guix-edit-manifest)
(with-eval-after-load 'evil
  ;; For some reason `global-set-key' does not work for Evil at this point.
  (dolist (mode '(normal insert))
    (evil-global-set-key mode (kbd "C-x c g") #'boogs/guix-edit-manifest)))

(defun boogs/guix-find-channel-from-manifest (pattern)
  "Return the channel specification file matching PATTERN in
`boogs/guix-channel-spec-directory'."
  (cl-first (directory-files boogs/guix-channel-spec-directory 'full
                          (concat pattern "-channel"))))

(defun boogs/run-in-eshell (command)
  (let ((eshell-buffer-name "*guix*"))
    (eshell)
    (when (eshell-interactive-process)
      (eshell t))
    (if (get-buffer-process (current-buffer))
        (message "Try again after current process termination.")
      (eshell-interrupt-process)
      (insert command)
      (eshell-send-input))))

(defun boogs/run-in-shell (command)
  (shell (get-buffer-create "*guix*"))
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (accept-process-output process 0.1)))
  (if (helm-ff-shell-alive-p major-mode)
      (message "Try again after current process termination.")
    (goto-char (point-max))
    (comint-delete-input)
    (insert command)
    (comint-send-input)))

(defun %boogs/guix-install-manifest (&optional manifests channel)
  "Install list of (NAME MANIFEST-FILE) using CHANNEL."
  (require 'init-shell)                ; For `helm-ff-preferred-shell-mode'.
  (let* ((guix (if channel
                   (let ((dest (expand-file-name
                                ;; TODO: What name should we use with multiple manifests?
                                (cl-first (cl-first manifests))
                                boogs/guix-extra-channels)))
                     (make-directory dest 'parents)
                     (format "guix pull --channels=%s --profile=%s/guix && %s/guix/bin/guix"
                             (shell-quote-argument channel)
                             (shell-quote-argument dest)
                             (shell-quote-argument dest)))
                 "guix")))
    (dolist (manifest-pair manifests)
      (let ((manifest-name (cl-first manifest-pair)))
        (make-directory (expand-file-name manifest-name
                                          boogs/guix-extra-profiles)
                        'parents)))
    (funcall (if (eq helm-ff-preferred-shell-mode 'eshell-mode)
                 'boogs/run-in-eshell
               'boogs/run-in-shell)
             (mapconcat #'identity
                        (mapcar (lambda (manifest-pair)
                                  (let ((manifest-name (cl-first manifest-pair))
                                        (manifest (cl-second manifest-pair))
                                        (profile (expand-file-name boogs/guix-extra-profiles)))
                                    (string-join
                                     (list "echo" (format "'==> Installing manifest %S to profile %S'"
                                                          manifest-name profile)
                                           ";"
                                      guix "package" (concat "--manifest=" manifest)
                                      (if (string= "default" manifest-name)
                                          ""
                                        (concat "--profile=" profile
                                                "/" manifest-name
                                                "/" manifest-name)))
                                     " ")))
                                manifests)
                        " ; "))))

(defun boogs/guix-install-manifest (&optional manifest channel)
  "Install Guix manifest to `boogs/guix-extra-profiles'.

Manifest is queried from those found in `boogs/guix-manifest-directory'.
Guix channel specification is stored in `boogs/guix-channel-spec-directory'.

With a prefix argument, query for a channel specification file.

If CHANNEL is nil and `boogs/guix-always-use-channel-specs' is
non-nil, then try to use a channel specification file from
`boogs/guix-channel-spec-directory' if any."
  (interactive)
  (let* ((manifest-pair (boogs/guix-query-manifest manifest))
         (manifest-name (cl-first manifest-pair))
         (manifest (cl-second manifest-pair))
         (channel (or channel
                      (and current-prefix-arg
                           (cl-second (boogs/guix-query-channel-spec)))
                      (and boogs/guix-always-use-channel-specs
                           (boogs/guix-find-channel-from-manifest manifest-name)))))
    (%boogs/guix-install-manifest (list manifest-pair) channel)))

(defun boogs/guix-install-manifests (&optional manifests channel)
  "Install Guix manifests to `boogs/guix-extra-profiles'.

Manifests are queried from those found in `boogs/guix-manifest-directory'.
Guix channel specification is stored in `boogs/guix-channel-spec-directory'.

With a prefix argument, query for a channel specification file."
  (interactive)
  (let* ((manifests (or manifests (boogs/guix-query-manifest nil :multiple)))
         (channel (or channel
                      (and current-prefix-arg
                           (cl-second (boogs/guix-query-channel-spec))))))
    (%boogs/guix-install-manifest manifests channel)))
(global-set-key (kbd "C-x c G") #'boogs/guix-install-manifests)
(with-eval-after-load 'evil
  (dolist (mode '(normal insert))
    (evil-global-set-key mode (kbd "C-x c G") #'boogs/guix-install-manifests)))

(defun boogs/guix-install-system (&optional system)
  "Install Guix system.

System is queried from those found in `boogs/guix-system-directory'. "
  (interactive)
  (require 'init-shell) ; For `helm-ff-preferred-shell-mode'.
  (let* ((system-pair (boogs/guix-query-system system))
         (system-name (cl-first system-pair))
         (system (cl-second system-pair)))
    (funcall (if (eq helm-ff-preferred-shell-mode 'eshell-mode)
                 'boogs/run-in-eshell
               'boogs/run-in-shell)
             (string-join
              (list "sudo" "-E"  "guix" "system" "-L" boogs/guix-system-directory
                    "reconfigure" system)
              " "))))
(global-set-key (kbd "C-x c S") #'boogs/guix-install-system)

(with-eval-after-load 'evil
  ;; For some reason `global-set-key' does not work for Evil at this point.
  (dolist (mode '(normal insert))
    (evil-global-set-key mode (kbd "C-x c S") #'boogs/guix-install-system)))

;; TODO: See `guix-apply-manifest' and expand on it.
;; TODO: Use --max-jobs=N.

(provide 'init-guix)
