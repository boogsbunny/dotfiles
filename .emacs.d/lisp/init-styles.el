;;--------------------------------;
;; Styles
;;--------------------------------;

;; (load-theme 'sanityinc-tomorrow-night t)
;; (load-theme 'gruvbox t)
;; (load-theme 'doom-spacegrey t)

;; (when (find-font (font-spec :name "Fira Mono"))
;; 	(add-to-list 'default-frame-alist '(font . "Fira Mono")))

;; (when window-system
;;   (if (< (x-display-pixel-width) 2000)
;;       (set-face-attribute 'default nil :font "Fira Mono" :height 130)
;;     (set-face-attribute 'default nil :font "Fira Mono" :height 250)))

;; (set-face-attribute 'default nil :font "Fira Mono" :height 250)
;; (when (find-font (font-spec :name "Fira Mono"))
;; 	(add-to-list 'default-frame-alist '(font . "Fira Mono")))

;;; General
;; (set-face-attribute 'default nil :foreground "white" :background "black")
(set-face-attribute 'default nil :foreground "white smoke" :background "#101010" :font "Fira Mono" :height 250)
(set-face-background 'mouse "#777777")  ; Darker mouse, less distracting.

;;; More readable but more space consuming; try on big screens.
; (setq-default line-spacing 1)

(set-face-background 'mode-line "white")
;; (set-face-foreground 'link "#00ffff")
(set-face-underline 'link t)
(set-face-foreground 'minibuffer-prompt "#00ffff")
(set-face-background 'region "#191970")
;; (set-face-attribute 'isearch nil :foreground 'unspecified :background "#2f4f4f" :box "white")
;; (set-face-attribute 'lazy-highlight nil :inherit 'isearch :foreground 'unspecified :background 'unspecified :box nil)
;;; TODO: Highlight with box does not render well in Sx, ediff, occur, evil-search.
;; (set-face-attribute 'highlight nil :background 'unspecified :box "white")
;; (set-face-attribute 'error nil :foreground "red" :weight 'bold)

;;; Line numbers.
;;; Graphic version has a gray bar separating text from line
;;; numbers, so we can leave the background black.
(if (display-graphic-p)
    (set-face-background 'shadow "black")
  (set-face-background 'shadow "#1c1c1c"))

;;; Whitespace mode
(with-eval-after-load 'whitespace
  (set-face-background 'whitespace-space-after-tab "#a9a9a9")
  (set-face-background 'whitespace-indentation "#696969"))

;;; Ediff
(with-eval-after-load 'ediff-init
  (set-face-attribute 'ediff-even-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-odd-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-even-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-odd-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-even-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-odd-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'ediff-current-diff-A nil :box "white")
  (set-face-attribute 'ediff-current-diff-B nil :box "white")
  (set-face-attribute 'ediff-current-diff-C nil :box "white"))

;;; Outline mode
(with-eval-after-load 'outline
  ;; (set-face-attribute 'outline-1 nil :inherit 'font-lock-warning-face)
  (set-face-attribute 'outline-1 nil :weight 'bold :foreground "#CBAC42")
  (set-face-attribute 'outline-2 nil :weight 'bold :foreground "#7BBF11")
  (set-face-attribute 'outline-3 nil :weight 'bold :foreground "#BC684F")
  (set-face-attribute 'outline-4 nil :weight 'bold :foreground "#4C95BF")
  (set-face-attribute 'outline-5 nil :weight 'bold :foreground "SeaGreen")
  (set-face-attribute 'outline-6 nil :weight 'bold :foreground "DarkSlateGray4")
  (set-face-attribute 'outline-7 nil :weight 'bold :foreground "DarkSlateBlue")
  (set-face-attribute 'outline-8 nil :weight 'bold :foreground "Gold"))

;;; show-paren
(with-eval-after-load 'paren
  ;; (set-face-background 'show-paren-match-face (face-background 'default)) ; Disable background color.
  (set-face-background 'show-paren-match "#555555")
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

;;; Man pages
(with-eval-after-load 'man
  (set-face-attribute 'Man-underline nil :foreground (face-foreground 'font-lock-string-face) :underline nil)
  (set-face-attribute 'Man-overstrike nil :foreground (face-foreground 'font-lock-comment-face) :weight 'normal))
(with-eval-after-load 'woman
  (set-face-foreground 'woman-bold (face-foreground 'font-lock-comment-face)))

;; Rainbow delimiters
(with-eval-after-load 'rainbow-delimiters
  ;; See https://yoo2080.wordpress.com/2013/09/08/living-with-rainbow-delimiters-mode/.
  ;; TODO: The color saturation metioned in the URL fails when running in daemon mode.
  ;; https://github.com/Fanael/rainbow-delimiters/issues/36
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#fe1717")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#589cff")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#f1fe52")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#44ff4c")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#83b2ff")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#6161ff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#35ff35")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#7ca8ff")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#50fec1")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t))

;; Helm
;; (with-eval-after-load 'helm
;;   (set-face-attribute 'helm-source-header nil :inherit 'header-line :height 'unspecified :background 'unspecified :foreground 'unspecified)
;;   (set-face-background 'helm-selection "#4f4f4f")
;;   (set-face-background 'helm-visible-mark "#2f2f2f")
;;   (set-face-foreground 'helm-visible-mark nil)
;;   (set-face-foreground 'helm-match "red")
;;   ;(set-face-attribute 'helm-buffer-file nil :background 'unspecified :foreground "white" :weight 'normal)
;;   (set-face-attribute 'helm-buffer-directory nil :background 'unspecified :foreground "#1e90ff" :weight 'bold)
;;   (set-face-attribute 'helm-ff-directory nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-directory)
;;   (set-face-attribute 'helm-ff-file nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-file)
;;   (set-face-foreground 'helm-grep-finish "#00AA00"))

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
