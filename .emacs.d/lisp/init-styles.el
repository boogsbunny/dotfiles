;;--------------------------------;
;; Styles
;;--------------------------------;

;; (load-theme 'sanityinc-tomorrow-night t)
;; (load-theme 'gruvbox t)
(load-theme 'doom-spacegrey t)

(when (find-font (font-spec :name "Fira Mono"))
	(add-to-list 'default-frame-alist '(font . "Fira Mono")))

(when window-system
  (if (< (x-display-pixel-width) 2000)
      (set-face-attribute 'default nil :font "Fira Mono" :height 170)
    (set-face-attribute 'default nil :font "Fira Mono" :height 300)))

;; (when (find-font (font-spec :name "Fira Mono"))
;; 	(add-to-list 'default-frame-alist '(font . "Fira Mono")))

;; transparency
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

(provide 'init-styles)
