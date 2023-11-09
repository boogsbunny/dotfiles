;;--------------------------------------------------------------------;
;; evil mode
;;--------------------------------------------------------------------;

(evil-mode 1)

(evil-select-search-module 'evil-search-module 'evil-search)

(remove-hook 'evil-insert-state-exit-hook 'expand-abbrev)

(setq undo-tree-mode-lighter "")

(setq evil-cross-lines t
      evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-want-fine-undo t
      evil-shift-width tab-width)

(setq-default evil-symbol-word-search t)

(when (require 'evil-commentary nil t)
  (evil-global-set-key 'normal "gc" 'evil-commentary)
  (evil-global-set-key 'normal "gy" 'evil-commentary-yank))

(evil-set-initial-state 'term-mode 'emacs)

(when (require 'with-editor nil t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(evil-global-set-key 'normal "gd" (lambda ()
                                    (interactive)
                                    (evil-execute-in-emacs-state)
                                    (call-interactively (key-binding (kbd "M-.")))))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

(define-key evil-insert-state-map (kbd "C-u") (lambda ()
                                                (interactive)
                                                (evil-delete (point-at-bol) (point))))

;; (define-key evil-normal-state-map (kbd "C-n") 'evil-prev-buffer)
;; (define-key evil-normal-state-map (kbd "C-p") 'evil-next-buffer)

;;(when (require 'evil-multiedit nil t)
;;  (global-set-key (kbd "C-;") 'evil-multiedit-match-all)
;;  (evil-multiedit-default-keybinds))

(setq evil-default-modeline-color (cons (face-background 'mode-line)
                                        (or (face-foreground 'mode-line) "black")))

(defun boogs/evil-color-modeline ()
  (let ((color (cond ((minibufferp) evil-default-modeline-color)
                     ((evil-insert-state-p) '("#006fa0" . "#ffffff"))
                     ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                     (t evil-default-modeline-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(setq evil-mode-line-format nil)

(setq evil-collection-setup-minibuffer t
      evil-collection-term-sync-state-and-mode-p t)
(when (require 'evil-collection nil t)
  (evil-collection-init))

;; (with-eval-after-load 'helm
;;   (define-key evil-motion-state-map "'" 'helm-all-mark-rings)
;;   (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;   (dolist (map (list helm-find-files-map helm-read-file-map))
;;     (boogs/define-keys map
;;                        "M-." 'helm-end-of-buffer
;;                        "M-," 'helm-beginning-of-buffer))
;;   (global-set-key (kbd "C-x C-x") 'helm-all-mark-rings))

(defun boogs/evil-notmuch (mode _mode-keymaps &rest _rest)
  (when (eq mode 'notmuch)
    (evil-define-key 'normal notmuch-hello-mode-map
      "s" 'helm-notmuch)
    (evil-define-key 'normal notmuch-search-mode-map
      "s" 'helm-notmuch)
    (evil-define-key 'normal notmuch-show-mode-map
      "s" 'helm-notmuch)))
(add-hook 'evil-collection-setup-hook 'boogs/evil-notmuch)

(with-eval-after-load 'magit
  (when (require 'evil-magit nil t)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "<" 'magit-section-up)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-j" 'magit-section-forward)
    (evil-magit-define-key evil-magit-state 'magit-mode-map "M-k" 'magit-section-backward)))

(with-eval-after-load 'org
  (when (require 'evil-org nil t)
    (add-hook 'org-mode-hook 'evil-org-mode)
    (evil-org-set-key-theme '(navigation textobjects additional shift))
    (defun boogs/evil-org-meta-return ()
      "Like `org-meta-return' but switch to insert mode."
      (interactive)
      (evil-insert 1)
      (org-meta-return))
    (evil-define-key 'normal evil-org-mode-map
      "^" 'org-up-element
      "<" 'org-up-element
      ">" 'org-down-element
      (kbd "M-<return>") 'boogs/evil-org-meta-return)
    (with-eval-after-load 'org-agenda
      (require 'evil-org-agenda)
      (evil-org-agenda-set-keys))
    (setq evil-collection-outline-bind-tab-p nil)

    (evil-define-key '(normal visual) evil-org-mode-map
      (kbd "<backtab>") 'org-shifttab)))

(require 'evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
(setq evil-escape-unordered-key-sequence 1)

;; ;; sane bindings
;; (defun boogs/jk ()
;;   (interactive)
;;   (let* ((initial-key ?j)
;;          (final-key ?k)
;;          (timeout 0.1)
;;          (event (read-event nil nil timeout)))
;;     (if event
;;         ;; timeout met
;;         (if (and (characterp event) (= event final-key))
;;             (evil-normal-state)
;;           (insert initial-key)
;;           (push event unread-command-events))
;;       ;; timeout exceeded
;;       (insert initial-key))))

;; (define-key evil-insert-state-map (kbd "j") 'boogs/jk)

;; (eval-after-load 'evil-maps
;;    '(progn
;;        (define-key evil-motion-state-map (kbd ";") 'evil-ex)))

(provide 'init-evil)
