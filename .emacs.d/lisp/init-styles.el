;;--------------------------------;
;; Styles
;;--------------------------------;

;; (load-theme 'solarized-dark t)
;; (load-theme 'apropospriate-dark t)
(load-theme 'sanityinc-tomorrow-night)

;; (require 'powerline)
;; (powerline-default-theme)

;; (require 'airline-themes)
;; (load-theme 'airline-doom-one t)

;; ;; font
;; ;; (when (find-font (font-spec :name -FBI -Input Mono-extralight-normal-normal-*-*-*-*-*-m-0-iso10646-1))
;; ;;   (add-to-list 'default-frame-alist '(font . "-FBI -Input Mono-extralight-normal-normal-*-*-*-*-*-m-0-iso10646-1")))

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
