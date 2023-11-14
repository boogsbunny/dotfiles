;;; init-modeline.el -*- lexical-binding: t -*-
;;--------------------------------------------------------------------
;; modeline
;;--------------------------------------------------------------------

(defgroup modeline nil
  ""
  :group 'mode-line)

(defgroup modeline-faces nil
  "Faces used in the custom modeline."
  :group 'modeline)

(defcustom modeline-string-truncate-length 9
  ""
  :type 'natnum)

;;; helpers
;;;###autoload
(defun boogs/common-window-small-p ()
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

(defun boogs/modeline--string-truncate-p (str)
  (and (boogs/common-window-small-p)
       (> (length str) modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun boogs/modeline--truncate-p ()
  (and (boogs/common-window-small-p)
       (not (one-window-p :no-minibuffer))))

(defun boogs/modeline-string-truncate (str)
  (if (boogs/modeline--string-truncate-p str)
      (concat (substring str 0 modeline-string-truncate-length) "...")
    str))

(defun boogs/modeline--first-char (str)
  (substring str 0 1))

(defun boogs/modeline-string-abbreviate (str)
  (if (boogs/modeline--string-truncate-p str)
      (mapconcat #'boogs/modeline--first-char (split-string str "[_-]") "-")
    str))

;;; faces
(defface modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-blue-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-cyan-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-green-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-magenta-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-red-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  ""
  :group 'modeline-faces)

(defface modeline-indicator-yellow-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  ""
  :group 'modeline-faces)

;;; alignment
(defun boogs/modeline--right-align-rest ()
  (format-mode-line
   `(""
     ,@(cdr (memq 'modeline-align-right mode-line-format)))))

(defun boogs/modeline--right-align-width ()
  (string-pixel-width (boogs/modeline--right-align-rest)))

(defun boogs/modeline--box-p ()
  (and (face-attribute 'mode-line :box)
       (null (eq (face-attribute 'mode-line :box) 'unspecified))))

(defun boogs/modeline--variable-pitch-p ()
  (when-let* ((mode-line-inherit (face-attribute 'mode-line :inherit))
              ((string-match-p "variable-pitch" (symbol-name mode-line-inherit)))
              (family-face (face-attribute mode-line-inherit :inherit))
              (variable-pitch
               (if (listp family-face)
                   (memq 'variable-pitch family-face)
                 (eq 'variable-pitch family-face))))
    variable-pitch))

(defun boogs/modeline--magic-number ()
  (let ((height (face-attribute 'mode-line :height nil 'default))
        (m-width (string-pixel-width (propertize "m" 'face 'mode-line))))
    (round height (* m-width (* height m-width 0.001)))))

(defvar-local modeline-align-right
    '(:eval
      (propertize
       " "
       'display
       (let ((box-p (boogs/modeline--box-p))
             (variable-pitch-p (boogs/modeline--variable-pitch-p))
             (magic-number (boogs/modeline--magic-number)))
         `(space
           :align-to
           (- right
              right-fringe
              right-margin
              ,(ceiling
                (boogs/modeline--right-align-width)
                (string-pixel-width (propertize "m" 'face 'mode-line)))
              ,(cond
                ((and variable-pitch-p box-p)
                 (* magic-number 0.5))
                ((and (not variable-pitch-p) box-p)
                 (* magic-number 0.25))
                ((and variable-pitch-p (not box-p))
                 0)
                (t (* magic-number -0.1)))))))))

;;; buffer status
(defvar-local modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight))))

;;; buffer identification
(defun boogs/modeline-buffer-identification-face ()
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun boogs/modeline--buffer-name ()
  (when-let ((name (buffer-name)))
    (boogs/modeline-string-truncate name)))

(defun boogs/modeline-buffer-name ()
  (let ((name (boogs/modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun boogs/modeline-buffer-name-help-echo ()
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local modeline-buffer-identification
    '(:eval
      (propertize (boogs/modeline-buffer-name)
                  'face (boogs/modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (boogs/modeline-buffer-name-help-echo))))

;;; evil
(defvar evil-state)
(defvar evil-visual-selection)

(defconst modeline-evil-state-tags
  '((normal     :short "<N>"   :long "NORMAL")
    (insert     :short "<I>"   :long "INSERT")
    (visual     :short "<V>"   :long "VISUAL")
    (vblock     :short "<Vb>"  :long "VBLOCK")
    (vline      :short "<Vl>"  :long "VLINE")
    (vsline     :short "<Vsl>" :long "VSLINE")
    (motion     :short "<M>"   :long "MOTION")))

(defun boogs/modeline--evil-get-tag (state variant)
  (let ((tags (alist-get state modeline-evil-state-tags)))
    (plist-get tags (or variant :short))))

(defun boogs/modeline--evil-get-format-specifier (variant)
  (if (eq variant :short)
      " %-5s"
    " %-8s"))

(defun boogs/modeline--evil-propertize-tag (state variant &optional face)
  (let ((tag (boogs/modeline--evil-get-tag state variant)))
    (propertize (format (boogs/modeline--evil-get-format-specifier variant) tag)
                'face (or face 'default)
                'mouse-face 'mode-line-highlight
                'help-echo (format "Evil `%s' state" state))))

(defun boogs/modeline-evil-state-tag (variant)
  (pcase evil-state
    ('normal (boogs/modeline--evil-propertize-tag 'normal variant 'modeline-indicator-blue))
    ('insert (boogs/modeline--evil-propertize-tag 'insert variant 'modeline-indicator-magenta))
    ('visual (pcase evil-visual-selection
               ('line (boogs/modeline--evil-propertize-tag 'vline variant 'modeline-indicator-yellow))
               ('screen-line (boogs/modeline--evil-propertize-tag 'vsline variant 'modeline-indicator-yellow))
               ('block (boogs/modeline--evil-propertize-tag 'vblock variant 'modeline-indicator-yellow))
               (_ (boogs/modeline--evil-propertize-tag 'visual variant 'modeline-indicator-yellow))))
    ('motion (boogs/modeline--evil-propertize-tag 'motion variant 'modeline-indicator-yellow))))

(defvar-local modeline-evil
    '(:eval
      (when (and (mode-line-window-selected-p) (bound-and-true-p evil-mode))
        (let ((variant (if (boogs/modeline--truncate-p) :short :long)))
          (boogs/modeline-evil-state-tag variant)))))

;;; major mode
(defun boogs/modeline-major-mode-indicator ()
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun boogs/modeline-major-mode-name ()
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun boogs/modeline-major-mode-help-echo ()
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local modeline-major-mode
    (list
     (propertize "%[" 'face 'modeline-indicator-red)
     '(:eval
       (concat
        (boogs/modeline-major-mode-indicator)
        " "
        (propertize
         (boogs/modeline-string-abbreviate
          (boogs/modeline-major-mode-name))
         'mouse-face 'mode-line-highlight
         'help-echo (boogs/modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'modeline-indicator-red)))

;;; narrow indicator
(defvar-local modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'modeline-indicator-cyan-bg))))

;;; vc
(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun boogs/modeline--vc-branch-name (file backend)
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map))

(defun boogs/modeline--vc-help-echo (file)
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun boogs/modeline--vc-text (file branch &optional face)
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (boogs/modeline--vc-help-echo file)
               'local-map modeline-vc-map)))

(defun boogs/modeline--vc-details (file branch &optional face)
  (boogs/modeline-string-truncate
   (boogs/modeline--vc-text file branch face)))

(defvar modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun boogs/modeline--vc-get-face (key)
   (alist-get key modeline--vc-faces 'up-to-date))

(defun boogs/modeline--vc-face (file backend)
  (boogs/modeline--vc-get-face (vc-state file backend)))

(defvar-local modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  (branch (boogs/modeline--vc-branch-name file backend))
                  (face (boogs/modeline--vc-face file backend)))
        (boogs/modeline--vc-details file branch face))))

;;; misc
(defvar-local modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info)))

;;; config
(dolist (construct '(modeline-narrow
                     modeline-buffer-status
                     modeline-evil
                     modeline-buffer-identification
                     modeline-major-mode
                     modeline-vc-branch
                     modeline-align-right
                     modeline-misc-info))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
      '("%e"
        modeline-narrow
        modeline-buffer-status
        modeline-evil
        modeline-buffer-identification
        " "
        modeline-major-mode
        " "
        modeline-vc-branch
        " "
        modeline-align-right
        " "
        modeline-misc-info))

;;; subtle
(defun boogs/modeline-set-faces (_theme)
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-active ((t :inherit mode-line :box unspecified)))
     `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified :overline ,subtle))))))

(defun boogs/modeline-unset-faces ()
  (custom-set-faces
   `(mode-line (( )))
   `(mode-line-active (( )))
   `(mode-line-inactive (( )))))

(defun boogs/modeline--enable-mode ()
  (boogs/modeline-set-faces nil)
  (add-hook 'enable-theme-functions #'boogs/modeline-set-faces))

(defun boogs/modeline--disable-mode ()
  (boogs/modeline-unset-faces)
  (remove-hook 'enable-theme-functions #'boogs/modeline-set-faces))

;;;###autoload
(define-minor-mode boogs/modeline-subtle-mode
  "Increase the padding/spacing of frames and windows."
  :global t
  (if boogs/modeline-subtle-mode
      (boogs/modeline--enable-mode)
    (boogs/modeline--disable-mode)))

(boogs/modeline-subtle-mode 1)

(provide 'init-modeline)
