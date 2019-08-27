;;--------------------------------;
;; Evil mode
;;--------------------------------;

(use-package evil
  :ensure t)
(evil-mode 1)

;; map 'jk' 'kj' to esc
(use-package key-chord
  :ensure t)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; map ";" to ":"
(eval-after-load 'evil-maps
	'(progn
		 (define-key evil-motion-state-map (kbd ";") 'evil-ex)))

;; Vim like shifting selected text to left "<<" or right ">>"
(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))
