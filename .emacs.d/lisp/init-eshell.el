;; ;;--------------------------------;
;; ;; Eshell
;; ;;--------------------------------;

;; ;;; Eshell gets initialized differently.  When eshell.el first gets loaded, only
;; ;;; the core is defined and `eshell-load-hook' is called. For every Eshell
;; ;;; session, `eshell-mode' is run: it resets `eshell-mode-map', it loads
;; ;;; modules, runs their hooks and concludes with `eshell-first-time-mode-hook'
;; ;;; (for the first session only) and `eshell-mode-hook'.

(add-hook 'eshell-mode-hook #'eat-eshell-mode)
(add-hook 'eshell-mode-hook #'eat-eshell-visual-command-mode)

;; Emacs pinentry for GPG.
(require 'init-defaults)

;; ;;; Use TRAMP to use Eshell as root.
(require 'em-tramp)
(setq password-cache t)
(setq password-cache-expiry 3600)

(setq
 eshell-ls-use-colors t
 ;; ffap-shell-prompt-regexp changes the behaviour of `helm-find-files' when
 ;; point is on prompt. I find this disturbing.
 ffap-shell-prompt-regexp nil
 eshell-history-size 1024
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t)

;; ;;; Alias management possibilities:
;; ;;; - Version eshell-alias and store it in user-emacs-directory. Simplest and
;; ;;; fastest, but aliases cannot be included conditionnaly, e.g. depending on the
;; ;;; existence of a program.
;; ;;; - Store eshell-alias in cache and populate it dynamically on startup.
;; ;; (setq eshell-aliases-file (concat user-emacs-directory "eshell-alias"))
;; ;;;
;; ;;; `eshell/alias' is too slow as it reads and write the file on each definition.
;; ;;; Let's write manually instead.
;; ;; (with-eval-after-load 'em-alias
;; ;;   ;;; If we read the alias list here, it means we make commandline-defined aliases persistent.
;; ;;   ;; (eshell-read-aliases-list)
;; ;;   (dolist
;; ;;       (alias
;; ;;        '(("l" "ls -1 $*")
;; ;;          ("la" "ls -lAh $*")
;; ;;          ("ll" "ls -lh $*")
;; ;;          ;; TODO: Aliasing eshell/{cp,mv,ln} does not work.
;; ;;          ;; REVIEW: Eshell/TRAMP's sudo does not work with aliases.
;; ;;          ;; See #28320, #27168.
;; ;;          ;; ("ls" "ls -F $*") ; not supported
;; ;;          ;; ("emacs" "find-file $1")
;; ;;          ;; ("cp" "eshell/cp -iv $*")
;; ;;          ;; ("mv" "eshell/mv -iv $*")
;; ;;          ("cpv" "cp -iv $*")
;; ;;          ("mvv" "mv -iv $*")
;; ;;          ("rmv" "rm -v $*")
;; ;;          ("md" "eshell/mkdir -p $*")
;; ;;          ("mkcd" "eshell/mkdir -p $* ; cd $1"))) ; TODO: '&&' does not work because mkdir exits with nil?
;; ;;     (add-to-list 'eshell-command-aliases-list alias))
;; ;;   (eshell-write-aliases-list))

;; ;;; Hooks
;; ;;; `nobreak-char-display' makes some output look weird, e.g. with 'tree'.
;; ;; (add-hook 'eshell-mode-hook 'boogs/turn-off-nobreak-char-display)

;; ;;; History
;; ;;; REVIEW: history: do not save failed Eshell commands (See `eshell-last-command-status')
;; ;;; Eshell commands always return 0.
;; (setq eshell-input-filter
;;       (lambda (str)
;;         (not (or
;;               ;; Here we can filter out failing commands.  This is usually a bad
;;               ;; idea since a lot of useful commands have non-zero exit codes
;;               ;; (including Emacs/Eshell functions).
;;               ;; (/= eshell-last-command-status 0)
;;               (string= "" str)
;;               (string= "cd" str)
;;               (string-prefix-p "cd " str)
;;               ;; Filter out space-beginning commands from history.
;;               (string-prefix-p " " str)))))

;; ;;; Shared history.
;; (defvar boogs/eshell-history-global-ring nil
;;   "The history ring shared across Eshell sessions.")

;; (defun boogs/eshell-hist-use-global-history ()
;;   "Make Eshell history shared across different sessions."
;;   (unless boogs/eshell-history-global-ring
;;     (when eshell-history-file-name
;;       (eshell-read-history nil t))
;;     (setq boogs/eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
;;   (setq eshell-history-ring boogs/eshell-history-global-ring))
;; (add-hook 'eshell-mode-hook 'boogs/eshell-hist-use-global-history)

;; ;; (defun boogs/eshell-history-remove-duplicates ()
;; ;;   (require 'functions) ; For `boogs/ring-delete-first-item-duplicates'.
;; ;;   (boogs/ring-delete-first-item-duplicates eshell-history-ring))
;; ;; (add-hook 'eshell-pre-command-hook 'boogs/eshell-history-remove-duplicates)

;; ;; Always save history
;; (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

;; ;;; Version and encrypt history.
;; ;; TODO: The following makes EPA always prompt for recipients for some reason.  Bug?
;; ;; (make-directory "~/projects/personal/history/" :parents)
;; ;; (setq eshell-history-file-name (expand-file-name "eshell.gpg" "~/projects/personal/history"))
;; ;; (defun boogs/fix-local-epa-file-encrypt-to ()
;; ;;   (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
;; ;;     (make-local-variable 'epa-file-encrypt-to))
;; ;;   (setq epa-file-encrypt-to "mail@boogs.xyz"))
;; ;; (add-hook 'eshell-mode-hook 'boogs/fix-local-epa-file-encrypt-to)

;; (when (require 'helm nil :noerror)
;;   (defun boogs/helm/eshell-set-keys ()
;;     (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
;;     (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
;;     (define-key eshell-mode-map (kbd "M-s") nil) ; Useless when we have 'helm-eshell-history.
;;     (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all))
;;   (add-hook 'eshell-mode-hook 'boogs/helm/eshell-set-keys))

;; ;;; Auto-suggestion
;; ;; (when (require 'esh-autosuggest nil t)
;; ;;   (setq esh-autosuggest-delay 0.75)
;; ;;   (add-hook 'eshell-mode-hook 'esh-autosuggest-mode)
;; ;;   (define-key esh-autosuggest-active-map (kbd "<tab>") 'company-complete-selection)
;; ;;   (when (require 'helm-config nil t)
;; ;;     ;; TODO: Why is this in esh-autosuggest?  Move to toplevel.
;; ;;     (define-key company-active-map (kbd "M-p") 'helm-eshell-history)))

;; ;;; Detach
;; ;; (when (require 'package-eshell-detach nil t)
;; ;;   (defun boogs/eshell-detach-set-keys ()
;; ;;     (define-key eshell-mode-map (kbd "C-c C-z") 'eshell-detach-stop)
;; ;;     (define-key eshell-mode-map (kbd "S-<return>") 'eshell-detach-send-input)
;; ;;     (define-key eshell-mode-map (kbd "C-<return>") 'eshell-detach-attach))
;; ;;   (add-hook 'eshell-mode-hook 'boogs/eshell-detach-set-keys))

;; ;; Man
;; ;; (when (string= (file-symlink-p (executable-find "man")) "mandoc")
;; ;;   ;; Some systems like Void Linux use mandoc instead of man and do not know the
;; ;;   ;; --nj, --nh flags.
;; ;;   (defun boogs/pcmpl-args-mandoc-man-function (name)
;; ;;     (let ((process-environment process-environment))
;; ;;       ;; Setting MANWIDTH to a high number makes most paragraphs fit on a single
;; ;;       ;; line, reducing the number of false positives that result from lines
;; ;;       ;; starting with `-' that aren't really options.
;; ;;       (push "MANWIDTH=10000" process-environment)
;; ;;       (pcmpl-args-process-file "man" "--" name)))
;; ;;   (setq pcmpl-args-man-function 'boogs/pcmpl-args-mandoc-man-function))

;; ;; Misc.
;; (defun boogs/eshell-append-region-to-command-line (begin end)
;;   (interactive "r")
;;   (require 'subr-x)                     ; For `string-trim'.
;;   (save-mark-and-excursion
;;     (let ((string (buffer-substring-no-properties begin end)))
;;       (setq string (string-trim string))
;;       (setq string (concat string " "))
;;       (goto-char (point-max))
;;       (ignore-errors (cycle-spacing 0))
;;       (insert string))))

;; ;; (when (require 'eshell-prompt-extras nil 'noerror)
;; ;;   ;; Leave `eshell-highlight-prompt' to t as it sets the read-only property.
;; ;;   (setq epe-path-style 'full)
;; ;;   (setq eshell-prompt-function #'epe-theme-multiline-with-status))

(require 'patch-eshell-inside-emacs)    ; Still required as of Emacs 27.1.

;; (defun boogs/eshell-narrow-to-prompt ()
;;   "Narrow buffer to prompt at point."
;;   (interactive)

;;   (narrow-to-region
;;    (save-excursion
;;      (forward-line)
;;      (call-interactively #'eshell-previous-prompt)
;;      (beginning-of-line)
;;      (point))
;;    (save-excursion
;;      (forward-line)
;;      (call-interactively #'eshell-next-prompt)
;;      (re-search-backward eshell-prompt-regexp nil t)
;;      (when (and (require 'eshell-prompt-extras nil 'noerror)
;;               (eq eshell-prompt-function #'epe-theme-multiline-with-status))
;;        (previous-line))
;;      (point))))

;; (defun boogs/eshell-set-keys ()
;;   (define-key eshell-mode-map (kbd "C-x n d") 'boogs/eshell-narrow-to-prompt))
;; (add-hook 'eshell-mode-hook 'boogs/eshell-set-keys)

(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         "["
         (if (string= (eshell/pwd) (getenv "HOME"))
             "~" (eshell/basename (eshell/pwd)))
         "]"
         (if (= (user-uid) 0) "# " "$ "))))

(provide 'init-eshell)
