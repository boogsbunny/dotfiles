;;--------------------------------;
;; Styles
;;--------------------------------;

; (load-theme 'sanityinc-tomorrow-night t)
(load-theme 'gruvbox t)

(when (find-font (font-spec :name "DejaVu Sans Mono-16"))
	(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-16")))

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
