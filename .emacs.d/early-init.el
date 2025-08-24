;; -*- lexical-binding: t -*-
;;--------------------------------------------------------------------
;; early-init
;;--------------------------------------------------------------------

;; additional config in 'lisp' folder
(add-to-list 'load-path (expand-file-name "patches/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;; site-lisp folder for local packages on MacOSX
;; (add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

(add-to-list 'exec-path "~/.npm-packages/bin")

;; separate user files with cache files
(setq user-emacs-directory "~/.cache/emacs/")

;; site-lisp folder for local packages on GNU/Linux
(defun boogs/package-refresh-load-path (path)
  "Add every non-hidden sub-folder of PATH to `load-path'."
  (when (file-directory-p path)
    (dolist (dir (directory-files path t "^[^\\.]"))
      (when (file-directory-p dir)
        (setq load-path (add-to-list 'load-path dir))
        (dolist (subdir (directory-files dir t "^[^\\.]"))
          (when (file-directory-p subdir)
            (setq load-path (add-to-list 'load-path subdir))))))))

(let ((site-lisp (expand-file-name "site-lisp/" "~/.local/share/emacs/")))
  (add-to-list 'load-path site-lisp)
  (boogs/package-refresh-load-path site-lisp))

(defun boogs/buffer-predicate (buffer)
  (if (or (string-match "helm" (buffer-name buffer))
          (string-match "Slack" (buffer-name buffer)))
      nil
    t))

(set-frame-parameter nil 'buffer-predicate 'boogs/buffer-predicate)

(setq evil-want-keybinding nil
      evil-want-integration t)

(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(setq max-lisp-eval-depth 10000)
(setq xref-search-program 'ripgrep)

(defvar hash-table-contains-p--sentinel
  (make-symbol "hash-table-contains-p--missing")
  "Sentinel used internally by `hash-table-contains-p'.")

(fset
 'hash-table-contains-p
 (lambda (key table)
   "Return non-nil if TABLE has an element with KEY."
   (declare (side-effect-free t) (important-return-value t))
   (not (eq (gethash key table hash-table-contains-p--sentinel)
            hash-table-contains-p--sentinel))))

(require 'init-packages)
