;;--------------------------------;
;; Styles
;;--------------------------------;

;; (use-package solarized-theme
;;   :ensure t)
;; (load-theme 'solarized-dark t)

;; (use-package powerline
;;   :ensure t)
;; (powerline-default-theme)
;(require 'powerline)
;(powerline-default-theme)

;(set-face-attribute 'default nil :foreground "white smoke" :background "#101010")
;(set-face-background 'mouse "#777777")  ; Darker mouse, less distracting.


;; (use-package airline-themes
;;   :ensure t)
;(require 'airline-themes)
;(load-theme 'airline-doom-one t)
;(setq powerline-height 30)

;; ;; font
;; ;; (when (find-font (font-spec :name -FBI -Input Mono-extralight-normal-normal-*-*-*-*-*-m-0-iso10646-1))
;; ;;   (add-to-list 'default-frame-alist '(font . "-FBI -Input Mono-extralight-normal-normal-*-*-*-*-*-m-0-iso10646-1")))

;; ;; transparency
;; (defun toggle-transparency ()
;;   (interactive)
;;   (let ((alpha (frame-parameter nil 'alpha)))
;;     (set-frame-parameter
;;      nil 'alpha
;;      (if (eql (cond ((numberp alpha) alpha)
;;                     ((numberp (cdr alpha)) (cdr alpha))
;;                     ;; Also handle undocumented (<active> <inactive>) form.
;;                     ((numberp (cadr alpha)) (cadr alpha)))
;;               100)
;;          '(85 . 50) '(100 . 100)))))
;; (global-set-key (kbd "C-c t") 'toggle-transparency)
(load-theme 'apropospriate-dark t)


(provide 'init-styles)
