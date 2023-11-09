;;--------------------------------------------------------------------;
;; elfeed -> emacs web feed reader
;;--------------------------------------------------------------------;

(setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
      elfeed-search-title-max-width 100)

;; The following is only useful if curl is not set up to always use Tor.
;; (when (member "tor"
;;               (mapcar (lambda (p) (alist-get 'comm (process-attributes p)))
;;                       (list-system-processes)))
;;   (setq elfeed-curl-extra-arguments '("--socks5" "localhost:9050")))

(defun boogs/elfeed-add-bookmark ()
  "Add bookmark using `eww-add-bookmark'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single))))
    (eww-add-bookmark (elfeed-entry-link entry)
                      (elfeed-entry-title entry))))
(define-key elfeed-search-mode-map "m" #'boogs/elfeed-add-bookmark)
(define-key elfeed-show-mode-map "m" #'boogs/elfeed-add-bookmark)

(defun boogs/elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): "
                                      '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defun boogs/elfeed-download-with-youtube-dl ()
  "Download entry with youtube-dl."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single)))
        (default-directory (expand-file-name "~/Downloads")))
    ;; TODO: Use ytdl.
    (message "Downloading %s with youtube-dl..." (elfeed-entry-link entry))
    (start-process "elfeed-youtube-dl" nil "youtube-dl" "-f" "best" (elfeed-entry-link entry))))

(defun boogs/elfeed-open-with-eww ()
  "Open in eww with `eww-readable'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single))))
    (eww  (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(defvar boogs/elfeed-visit-patterns
  '(("//[^/]*youtu\\.?be" . boogs/elfeed-download-with-youtube-dl)
    ("phoronix" . boogs/elfeed-open-with-eww))
  "List of (regexps . function) to match against elfeed entry link to know
whether how to visit the link.")

(defun boogs/elfeed-visit-maybe-external ()
  "Visit with external function if entry link matches `boogs/elfeed-visit-patterns',
visit otherwise."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single)))
        (patterns boogs/elfeed-visit-patterns))
    (while (and patterns (not (string-match (caar patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (cond
     (patterns
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (funcall (cdar patterns)))
     ((eq major-mode 'elfeed-search-mode)
      (call-interactively 'elfeed-search-show-entry))
     (t (elfeed-show-visit)))))

(defun boogs/elfeed-kill-entry ()
  "Like `elfeed-kill-buffer' but pop elfeed search."
  (interactive)
  (elfeed-kill-buffer)
  (switch-to-buffer "*elfeed-search*"))
(define-key elfeed-show-mode-map "q" #'boogs/elfeed-kill-entry)

;; Show entry in different buffers.
;; REVIEW: Reported upstream:
;; https://github.com/skeeto/elfeed/issues/307
;; Merged, remove the advice in Elfeed >3.0.0.
(defvar boogs/elfeed-show-unique-buffers t)
(defun boogs/elfeed-show-entry (entry)
  "Display ENTRY in the current buffer."
  (let ((buff (get-buffer-create (if boogs/elfeed-show-unique-buffers
                                     (format "*elfeed-entry-<%s>*"
                                             (elfeed-entry-title entry))
                                   "*elfeed-entry*"))))
    (with-current-buffer buff
      (elfeed-show-mode)
      (setq elfeed-show-entry entry)
      (elfeed-show-refresh))
    (funcall elfeed-show-entry-switch buff)))
(advice-add 'elfeed-show-entry :override 'boogs/elfeed-show-entry)

(defun boogs/elfeed-setup ()
  (add-hook 'window-configuration-change-hook 'elfeed-search-update--force nil t))
;; This is too slow on big buffers.
;; (add-hook 'elfeed-search-mode-hook 'boogs/elfeed-setup)

(load (expand-file-name "news/elfeed.el.gpg" (getenv "PERSONAL")) :noerror)

(provide 'init-elfeed)
