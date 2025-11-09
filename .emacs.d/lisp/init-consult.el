;;--------------------------------------------------------------------
;; consult
;;--------------------------------------------------------------------

(require 'consult)
(require 'consult-imenu)

(defun consult-ripgrep-up-directory ()
  (interactive)
  (let ((parent-dir (file-name-directory (directory-file-name default-directory))))
    (when parent-dir
        (run-at-time 0 nil
                     #'consult-ripgrep
                     parent-dir
                     (ignore-errors
                       (buffer-substring-no-properties
                        (1+ (minibuffer-prompt-end)) (point-max))))))
  (minibuffer-quit-recursive-edit))

(consult-customize
 consult-ripgrep
 :keymap (let ((map (make-sparse-keymap)))
           (define-key map (kbd "M-l") #'consult-ripgrep-up-directory)
           map))

(defun boogs/consult-grep-git-or-rg (arg)
  (interactive "P")
  (require 'vc)
  (require 'functions)
  (if (and (vc-find-root default-directory ".git")
           (or arg (split-string (boogs/call-process-to-string "git" "ls-files" "-z") "\0" t)))
      (consult-ripgrep arg)
    (consult-ripgrep)))

(setq consult-async-min-input 3)
(setq consult-async-input-debounce 0.5)
(setq consult-async-input-throttle 0.8)

(setq consult-find-args
      (concat "find . -not ( "
              "-path */.git* -prune "
              "-or -path */.cache* -prune )"))

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(setq consult-narrow-key "<")

(define-key minibuffer-mode-map (kbd "M-p") 'consult-history)

(defvar-local consult-toggle-preview-orig nil)

(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (progn
        (setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
        (message "Live preview is enabled"))
    (progn
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)
      (message "Live preview is disabled"))))

(define-key vertico-map (kbd "C-c C-f") #'consult-toggle-preview)

(defun consult-exwm-preview-fix (&rest _args)
  "Kludge to stop EXWM buffers from stealing focus during Consult previews."
  (when (derived-mode-p 'exwm-mode)
    (when-let ((mini (active-minibuffer-window)))
      (select-window (active-minibuffer-window)))))

(advice-add
    #'consult--buffer-preview :after #'consult-exwm-preview-fix)

;; (setq consult-preview-excluded-buffers '(major-mode . exwm-mode))

(defvar +consult-exwm-filter "\\`\\*EXWM")
(add-to-list 'consult-buffer-filter +consult-exwm-filter)

(defvar +consult-source-exwm
  `(:name      "EXWM"
    :narrow    ?x
    ;; :hidden t
    :category  buffer
    :face      consult-buffer
    :history   buffer-name-history
    ;; Specify either :action or :state
    ;; :action    ,#'consult--buffer-action ;; No preview
    :state  ,#'consult--buffer-state  ;; Preview
    :items
    ,(lambda () (consult--buffer-query
                 :sort 'visibility
                 :as #'buffer-name
                 :exclude (remq +consult-exwm-filter consult-buffer-filter)
                 :mode 'exwm-mode)))
  "EXWM buffer source.")

(add-to-list 'consult-buffer-sources '+consult-source-exwm 'append)

(consult-customize consult--source-buffer :hidden t :default nil)

(defvar consult--source-perspective
  (list :name     "Perspective"
        :narrow   ?s
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    #'persp-get-buffer-names))

;; (push consult--source-perspective consult-buffer-sources)
;; (add-to-list 'consult-buffer-sources 'consult--source-perspective 'append)

(defcustom boogs/consult-ripgrep-or-line-limit 300000
  "Buffer size threshold for `boogs/consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
  :type 'integer)

(defun boogs/consult-ripgrep-or-line ()
  "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
  (interactive)
  (if (or (not buffer-file-name)
          (buffer-narrowed-p)
          (ignore-errors
            (file-remote-p buffer-file-name))
          (jka-compr-get-compression-info buffer-file-name)
          (<= (buffer-size)
              (/ boogs/consult-ripgrep-or-line-limit
                 (if (eq major-mode 'org-mode) 4 1))))
      (consult-line)
    (when (file-writable-p buffer-file-name)
      (save-buffer))
    (let ((consult-ripgrep-args
           (concat consult-ripgrep-args
                   ;; filter to desired filename
                   " -g "
                   (shell-quote-argument (file-name-nondirectory buffer-file-name))
                   " ")))
      (consult-ripgrep))))

(defun boogs/consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal
                   :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

;; (setq read-file-name-function #'boogs/consult-find-file-with-preview)

(defun boogs/consult-project-find-file-with-preview (prompt all-files &optional pred hist _mb)
  (let ((prompt (if (and all-files
                         (file-name-absolute-p (car all-files)))
                    prompt
                  ( concat prompt
                    ( format " in %s"
                      (consult--fast-abbreviate-file-name default-directory)))))
        (minibuffer-completing-file-name t))
    (consult--read (mapcar
                    (lambda (file)
                      (file-relative-name file))
                    all-files)
                   :state (consult--file-preview)
                   :prompt (concat prompt ": ")
                   :require-match t
                   :history hist
                   :category 'file
                   :predicate pred)))

(setq project-read-file-name-function #'boogs/consult-project-find-file-with-preview)

(provide 'init-consult)
