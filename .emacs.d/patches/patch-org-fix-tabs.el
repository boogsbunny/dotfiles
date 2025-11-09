(require 'org)

;; Helper function to convert existing Org documents to the new
;; standard tab width.
(defun org-compat-adjust-tab-width-in-buffer (old-width)
  "Adjust visual indentation from `tab-width' equal OLD-WIDTH to 8."
  (interactive "nOld `tab-width': ")
  (cl-assert (derived-mode-p 'org-mode))
  (unless (= old-width 8)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (bound
           (repl (if (< old-width 8)
                     (make-string old-width ?\s)
                   (concat "\t" (make-string (- old-width 8) ?\s)))))
       (while (re-search-forward "^ *\t" nil t)
         (skip-chars-forward " \t")
         (setq bound (point-marker))
         (forward-line 0)
         (while (search-forward "\t" bound t)
           (replace-match repl)))))))

(defun fix-org-tabs-in-directory (dir)
  "Fix tab width in all org files under DIR."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir "\\.org$"))
        (tab-width 8))  ; Set tab width to 8
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (message "Processing %s" file)
        (org-mode)
        (org-compat-adjust-tab-width-in-buffer 2)
        (save-buffer)
        (kill-buffer)))))
