;;--------------------------------;
;; Styles
;;--------------------------------;

(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-dark t)

(use-package powerline
  :ensure t)
(powerline-default-theme)

(use-package airline-themes
  :ensure t)
(load-theme 'airline-doom-one t)
(setq powerline-height 30)

;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 50)))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
