;;--------------------------------------------------------------------
;; functions
;;--------------------------------------------------------------------

(defun boogs/call-process-to-string (program &rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file program nil t nil args))))

(defun boogs/local-set-keys (key def &rest bindings)
  "Like `local-set-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (local-set-key (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun boogs/tabify-leading ()
  "Call `tabify' on leading spaces only.
Works on whole buffer if region is unactive."
  (interactive)
  (require 'tabify) ; Need this to initialize `tabify-regexp'.
  (let ((tabify-regexp-old tabify-regexp) start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq start (point-min) end (point-max)))
    (unwind-protect
        (progn
          (setq tabify-regexp "^\t* [ \t]+")
          (tabify start end))
      (setq tabify-regexp tabify-regexp-old))))

(defun boogs/prettify ()
  "(Un)tabify, indent and delete trailing whitespace.

Tabify if `indent-tabs-mode' is true, otherwise use spaces.
Work on buffer or region.

Require `boogs/tabify-leading'."
  (interactive)
  (let ((start (set-marker (make-marker)
                           (if (use-region-p)
                               (region-beginning)
                             (point-min))))
        (end (set-marker (make-marker)
                         (if (use-region-p)
                             (region-end)
                           (point-max)))))
    (if indent-tabs-mode
        (boogs/tabify-leading)
      (untabify start end))
    (indent-region start end)
    (save-restriction
      (narrow-to-region start end)
      (delete-trailing-whitespace))))

(defun boogs/define-keys (map key def &rest bindings)
  "Like `define-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (define-key map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun boogs/ring-delete-first-item-duplicates (ring)
  "Remove duplicates of last command in history.
Return RING.

This should be faster then `seq-uniq'.  Unlike
`eshell-hist-ignoredups' or `comint-input-ignoredups', it does
not allow duplicates ever.
Surrounding spaces are ignored when comparing."
  (let ((first (ring-ref ring 0))
        (index 1))
    (while (<= index (1- (ring-length ring)))
      (if (string= (string-trim first)
                   (string-trim (ring-ref ring index)))
          ;; REVIEW: We could stop at the first match, it would be
          ;; faster and it would eliminate duplicates if we started
          ;; from a fresh history. From an existing history that would
          ;; not clean up existing duplicates beyond the first one.
          (ring-remove ring index)
        (setq index (1+ index))))
    ring))

(provide 'functions)
