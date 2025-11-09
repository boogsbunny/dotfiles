;;--------------------------------------------------------------------
;; projectile-perspective
;;--------------------------------------------------------------------

;;;###autoload
(defun boogs/project-vc-dir ()
  "Run `magit-status' in the current project's root."
  (interactive)
  (magit-status (project-root (project-current t))))

(define-key project-prefix-map (kbd "m") 'boogs/project-vc-dir)

;;;###autoload
(defun boogs/project-persp-vc-dir ()
  "Create/switch to a perspective named after the project repo and current
Git branch.

Handles Git worktrees:
Use the real repo name (parent of the shared .git) and append a short
branch (8 chars, '/' -> '-'), unless the worktree folder is already
named like REPO-BRANCH. Then open Magit."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (common-raw (shell-command-to-string
                      "git rev-parse --git-common-dir 2>/dev/null"))
         (common (let* ((s (replace-regexp-in-string
                            "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" ""
                            common-raw)))
                   (if (string= s "")
                       nil
                     (expand-file-name s default-directory))))
         (repo-dir (when common
                     (directory-file-name
                      (if (string-match-p "/\\.git\\'" common)
                          (file-name-directory common)
                        common))))
         (repo-name (if repo-dir
                        (file-name-nondirectory repo-dir)
                      (file-name-nondirectory
                       (directory-file-name project-root))))
         (worktree-name (file-name-nondirectory
                         (directory-file-name project-root)))
         (branch (or (magit-get-current-branch)
                     (let* ((raw (shell-command-to-string
                                  "git rev-parse --short HEAD 2>/dev/null"))
                            (s (replace-regexp-in-string
                                "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" raw)))
                       (unless (string= s "") s))))
         (short-branch (when branch
                         (substring branch 0 (min 8 (length branch)))))
         (sbranch (when short-branch
                    (replace-regexp-in-string "/" "-" short-branch)))
         (composed (if sbranch
                       (format "%s-%s" repo-name sbranch)
                     repo-name))
         (persp-name
          (if (and sbranch
                   (string-match-p
                    (regexp-quote (format "%s-%s" repo-name sbranch))
                    worktree-name))
              worktree-name
            composed)))
    (message "Switching to perspective: %s" persp-name)
    (persp-switch persp-name)
    (magit-status project-root)))

(define-key project-prefix-map (kbd "s") 'boogs/project-persp-vc-dir)

(setq project-switch-commands '((project-find-file "Find file")
                                (project-find-regexp "Find regexp")
                                (project-find-dir "Find directory")
                                (boogs/project-vc-dir "Magit")
                                (boogs/project-persp-vc-dir "Start Project")
                                (project-eshell "Eshell")))

(provide 'init-projectile-perspective)
