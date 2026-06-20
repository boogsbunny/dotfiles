;;--------------------------------------------------------------------
;; olivetti
;;--------------------------------------------------------------------

(require 'olivetti)

(setq olivetti-recall-visual-line-mode-entry-state t)

(defun single-buffer-visible-p ()
  "Non-nil if only a single window is visible in the selected frame."
  (<= (length (window-list (selected-frame))) 1))

(defun boogs/single-window-visible-p (&optional frame)
  "Non-nil if FRAME has exactly one live non-minibuffer window."
  (= (length (window-list frame 'no-minibuffer)) 1))

(defun laptop-screen-p ()
  (and
   (<= (display-pixel-width) 3000)
   (<= (display-pixel-height) 2000)))

(defconst boogs/olivetti-eligible-modes
  '(comint-mode
    dired-mode
    elfeed-show-mode
    erc-mode
    eww-mode
    json-mode
    magit-status-mode
    markdown-mode
    notmuch-message-mode
    notmuch-show-mode
    org-agenda-mode
    org-mode
    prog-mode
    slack-mode
    speed-type-mode
    text-mode
    yaml-mode)
  "Major modes in which Olivetti may be auto-enabled.")


(defun boogs/olivetti-eligible-buffer-p (&optional buffer)
  "Return non-nil if BUFFER's major mode is eligible for Olivetti."
  (with-current-buffer (or buffer (current-buffer))
    (apply #'derived-mode-p boogs/olivetti-eligible-modes)))

(defun boogs/olivetti-should-enable-p (&optional buffer)
  "Return non-nil if Olivetti should be enabled in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (and
     (boogs/single-window-visible-p (selected-frame))
     (boogs/olivetti-eligible-buffer-p))))

(defun boogs/olivetti-apply-current-buffer ()
  "Lazily enable or disable Olivetti in the current buffer only."
  (unless (minibufferp)
    (let ((want (boogs/olivetti-should-enable-p)))
      (unless (eq want (bound-and-true-p olivetti-mode))
        (when want
          (setq-local olivetti-body-width 0.65)
          (setq-local olivetti-minimum-body-width 140))
        (olivetti-mode (if want 1 -1))))))

(defun boogs/olivetti-apply-on-window-selection-change (&rest _)
  (boogs/olivetti-apply-current-buffer))

(add-hook 'window-selection-change-functions
          #'boogs/olivetti-apply-on-window-selection-change)

(add-hook 'after-change-major-mode-hook
          #'boogs/olivetti-apply-current-buffer)

(provide 'init-olivetti)
