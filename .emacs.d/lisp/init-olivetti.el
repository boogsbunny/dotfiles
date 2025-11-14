;;--------------------------------------------------------------------
;; olivetti
;;--------------------------------------------------------------------

(require 'olivetti)

(setq olivetti-recall-visual-line-mode-entry-state t)

(defun single-buffer-visible-p ()
  "Non-nil if only a single window is visible in the selected frame."
  (<= (length (window-list (selected-frame))) 1))

(defun laptop-screen-p ()
  (and
   (<= (x-display-pixel-width) 3000)
   (<= (x-display-pixel-height) 2000)))

(defconst boogs/olivetti-eligible-modes
  '(comint-mode
    dired-mode
    elfeed-show-mode
    erc-mode
    eww-mode
    json-mode
    magit-status-mode
    markdown-mode
    org-mode
    prog-mode
    slack-mode
    text-mode
    yaml-mode)
  "Major modes in which Olivetti may be auto-enabled.")

(defun boogs/olivetti-should-enable-p ()
  (and (not (laptop-screen-p))
       (single-buffer-visible-p)
       (apply #'derived-mode-p boogs/olivetti-eligible-modes)))

(defun maybe-enable-olivetti-mode ()
  "Enable/disable Olivetti only when the desired state changes,
preserving width."
  (let ((want (boogs/olivetti-should-enable-p)))
    (unless (eq want olivetti-mode)
      (when want
        (setq-local olivetti-body-width 0.65)
        (setq-local olivetti-minimum-body-width 140))
      (olivetti-mode (if want 1 -1)))))

(add-hook 'window-configuration-change-hook #'maybe-enable-olivetti-mode)
(add-hook 'after-change-major-mode-hook #'maybe-enable-olivetti-mode)

;; Avoid expensive margin work during buffer kills.
(remove-hook 'kill-buffer-hook #'maybe-enable-olivetti-mode)

(provide 'init-olivetti)
