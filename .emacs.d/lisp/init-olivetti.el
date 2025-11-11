;;--------------------------------------------------------------------
;; olivetti
;;--------------------------------------------------------------------

(defun single-buffer-visible-p ()
  "Check if only a single buffer is visible in the current frame."
  (<= (length (window-list)) 1))

(defun laptop-screen-p ()
  (and
   (<= (x-display-pixel-width) 3000)
   (<= (x-display-pixel-height) 2000)))

(defun maybe-enable-olivetti-mode ()
  "Enable olivetti mode if on monitor and only a single buffer is visible."
  (if (and (not (laptop-screen-p))
           (or (and (derived-mode-p 'comint-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'dired-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'erc-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'eww-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'json-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'magit-status-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'markdown-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'text-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'elfeed-show-mode) (single-buffer-visible-p))
               ;; (and (derived-mode-p 'notmuch-show-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'org-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'slack-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'yaml-mode) (single-buffer-visible-p))
               (and (derived-mode-p 'prog-mode) (single-buffer-visible-p))))
      (olivetti-mode 1)
    (olivetti-mode -1)))

(add-hook 'window-configuration-change-hook 'maybe-enable-olivetti-mode)
(add-hook 'kill-buffer-hook 'maybe-enable-olivetti-mode)
(add-hook 'after-change-major-mode-hook 'maybe-enable-olivetti-mode)

(setq olivetti-body-width 0.65
      olivetti-minimum-body-width 72
      olivetti-recall-visual-line-mode-entry-state t)

(provide 'init-olivetti)
