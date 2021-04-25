;;; EXWM

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; force-kill Emacs before it can run through `kill-emacs-hook'.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;;; means that `C-` keys will works but `<escape>` will need a fast double tap
;;; on Caps Lock.
;;;
;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;;; Rename buffer to window title.
(defun boogs/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'boogs/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
;
(when (require 'windower nil 'noerror)
  (exwm-input-set-key (kbd "s-<tab>") 'windower-switch-to-last-buffer)
  (exwm-input-set-key (kbd "s-o") 'windower-toggle-single)
  (exwm-input-set-key (kbd "s-\\") 'windower-toggle-split)
  (exwm-input-set-key (kbd "s-H") 'windower-swap-left)
  (exwm-input-set-key (kbd "s-J") 'windower-swap-below)
  (exwm-input-set-key (kbd "s-K") 'windower-swap-above)
  (exwm-input-set-key (kbd "s-L") 'windower-swap-right))

;(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(with-eval-after-load 'helm
  (global-set-key (kbd "s-b") #'helm-mini)
  (global-set-key (kbd "s-f") #'helm-find-files)
  ;; (push `(,(kbd "s-f") . helm-find-files) exwm-input-global-keys)
  (exwm-input-set-key (kbd "s-F") #'helm-locate)
  (when (fboundp 'boogs/helm-locate-meta)
    (exwm-input-set-key (kbd "s-F") #'boogs/helm-locate-meta))
  (exwm-input-set-key (kbd "s-g") 'boogs/helm-grep-git-or-ag)
  (exwm-input-set-key (kbd "s-G") 'boogs/helm-grep-git-all-or-ag)
  ;; Launcher
  (exwm-input-set-key (kbd "s-r") 'helm-run-external-command))

(when (require 'evil nil t)
  (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
  (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

;;; Emacs mode shortcuts.
(autoload 'boogs/helm-selector-sly "init-sly")
(if (not (require 'helm-selector nil :noerror))
    (progn
      (exwm-input-set-key (kbd "s-t") (lambda ()
                                        (interactive)
                                        (find-file (car org-agenda-files))))
      (exwm-input-set-key (kbd "s-<return>") #'eshell)
      (exwm-input-set-key (kbd "s-m") #'notmuch-hello)
      (exwm-input-set-key (kbd "s-n") #'elfeed)
      (exwm-input-set-key (kbd "s-e") #'eww))
  (exwm-input-set-key (kbd "s-t") 'helm-selector-org)
  (exwm-input-set-key (kbd "s-T") 'helm-selector-org-other-window)
  (exwm-input-set-key (kbd "s-<return>") 'boogs/helm-selector-sly)
  (exwm-input-set-key (kbd "S-s-<return>") 'boogs/helm-selector-sly-other-window)
  (exwm-input-set-key (kbd "s-m") #'helm-selector-notmuch)
  (exwm-input-set-key (kbd "s-M") #'helm-selector-notmuch-other-window)
  (exwm-input-set-key (kbd "s-n") #'helm-selector-elfeed)
  (exwm-input-set-key (kbd "s-N") #'helm-selector-elfeed-other-window) ; "n" for "news"
  (exwm-input-set-key (kbd "s-e") #'helm-selector-eww)
  (exwm-input-set-key (kbd "s-E") #'helm-selector-eww-other-window))

(when (fboundp 'magit-status)
  (if (require 'helm-selector-magit nil :noerror)
      (progn
        (exwm-input-set-key (kbd "s-v") #'helm-selector-magit)
        (exwm-input-set-key (kbd "s-V") #'magit-status))
    (exwm-input-set-key (kbd "s-v") #'magit-status)))
(when (fboundp 'emms-all)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "S-s-<kp-enter>") #'emms-pause)
  (if (fboundp 'helm-emms)
      (exwm-input-set-key (kbd "s-A") #'helm-emms)
    (exwm-input-set-key (kbd "s-A") #'emms)))

(when (fboundp 'helm-pass)
  (exwm-input-set-key (kbd "s-p") #'helm-pass))

(autoload 'boogs/slime-to-repl "lisp")
(exwm-input-set-key (kbd "s-<backspace>") #'helm-selector-sly)
(defun boogs/repl-switcher ()
  "Switch between Geiser and SLIME REPLs."
  (interactive)
  ;; TODO: Apparently, S-s-<backspace> is not recognized.
  (pcase
      (completing-read "Lisp: " '(cider geiser slime sly racket))
    ("cider"
     (exwm-input-set-key (kbd "s-<backspace>") 'helm-selector-cider)
     (exwm-input-set-key (kbd "M-s-<backspace>") 'helm-selector-cider-other-window))
    ("geiser"
     (autoload 'helm-geiser-repl-switch "init-scheme")
     (exwm-input-set-key (kbd "s-<backspace>") 'helm-selector-geiser)
     (exwm-input-set-key (kbd "M-s-<backspace>") 'helm-selector-geiser-other-window))
    ("slime"
     (exwm-input-set-key (kbd "s-<backspace>") 'helm-selector-slime)
     (exwm-input-set-key (kbd "M-s-<backspace>") 'helm-selector-slime-other-window))
    ("sly"
     (exwm-input-set-key (kbd "s-<backspace>") 'helm-selector-sly)
     (exwm-input-set-key (kbd "<M-s-backspace>") 'helm-selector-sly-other-window))
    ("racket"
     (exwm-input-set-key (kbd "s-<backspace>") #'racket-repl))))
(exwm-input-set-key (kbd "s-C-<backspace>") #'boogs/repl-switcher)

;;; External application shortcuts.
(defun boogs/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'boogs/exwm-start)

(when (require 'helm-exwm nil t)
  (add-to-list 'helm-source-names-using-follow "EXWM buffers")
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf
                                    ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
  ;; Web browser
  (exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
  (exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window))

(when (require 'desktop-environment nil 'noerror)
  ;; REVIEW: Remove the override on next version release:
  ;; https://gitlab.petton.fr/DamienCassou/desktop-environment
  (defun boogs/desktop-environment-lock-screen ()
    "Lock the screen, preventing anyone without a password from using the system."
    (interactive)
    ;; Run command asynchronously so that Emacs does not wait in the background.
    (start-process-shell-command "lock" nil desktop-environment-screenlock-command))
  (advice-add #'desktop-environment-lock-screen :override #'boogs/desktop-environment-lock-screen)
  (setq desktop-environment-screenshot-directory "~/Downloads")
  (desktop-environment-mode))

(defun boogs/suspend-to-sleep ()
  (interactive)
  (require 'recentf)
  (recentf-save-list)
  (call-process "loginctl" nil nil nil "suspend"))
(exwm-input-set-key (kbd "s-Z") #'boogs/suspend-to-sleep)

;;; Volume control
(when (require 'pulseaudio-control nil t)
  (exwm-input-set-key (kbd "s-<kp-subtract>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-<kp-add>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-<kp-enter>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute))

;;; Check for start-up errors. See ~/.profile.
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

;;; Some programs such as 'emacs' are better off being started in char-mode.
(defun boogs/exwm-start-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'boogs/exwm-start-in-char-mode)

;;; Some programs escape EXWM control and need be tamed.  See
;; https://github.com/ch11ng/exwm/issues/287
(add-to-list 'exwm-manage-configurations '((string= exwm-title "Kingdom Come: Deliverance") managed t))

;; Gens
(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "Gens") floating nil))

;; Askpass.
(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "lxqt-openssh-askpass") floating t))

(defvar boogs/exwm-change-screen-turn-off-primary nil
  "Turn off primary display when cable is plugged.")
;; Function to automatically toggle between internal/external screens.
(defun boogs/exwm-change-screen-hook ()
  (let* ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
         (xrandr-monitor-regexp "\n .* \\([^ \n]+\\)")
         (find-first-output (lambda ()
                              (call-process "xrandr" nil t nil)
                              (goto-char (point-min))
                              (re-search-forward xrandr-output-regexp nil 'noerror)
                              (match-string 1)))
         (default-output (with-temp-buffer
                           (funcall find-first-output)))
         (second-output (with-temp-buffer
                          (funcall find-first-output)
                          (forward-line)
                          (when (re-search-forward xrandr-output-regexp nil 'noerror)
                            (match-string 1)))))
    (if (not second-output)
        (progn
          (call-process "xrandr" nil nil nil "--output" default-output "--auto" "--primary")
          (with-temp-buffer
            ;; Turn off all monitors that are not DEFAULT-OUTPUT.
            ;; See "--auto" in xrandr(1) and https://github.com/ch11ng/exwm/issues/529.
            (call-process "xrandr" nil t nil "--listactivemonitors")
            (goto-char (point-min))
            (while (not (eobp))
              (when (and (re-search-forward xrandr-monitor-regexp nil 'noerror)
                         (not (string= (match-string 1) default-output)))
                (call-process "xrandr" nil nil nil "--output" (match-string 1) "--auto")))))
      (apply #'call-process
             "xrandr" nil nil nil
             "--output" second-output
             "--auto"
             (append
              (if boogs/exwm-change-screen-turn-off-primary '("--primary") '())
              (when boogs/exwm-change-screen-turn-off-primary
                (list "--output" default-output "--off"))))
      (setq exwm-randr-workspace-monitor-plist (list 0 second-output)))))

(require 'exwm-randr)
(exwm-randr-enable)

(defun boogs/exwm-change-screen-toggle (&optional setting)
  "Toggle automatic multiscreen configuration.
Turn off if you want to set the display settings manually.
WIth SETTING to :ENABLE or :DISABLE, set"
  (interactive)
  (if (member 'boogs/exwm-change-screen-hook exwm-randr-screen-change-hook)
      (progn
        (remove-hook 'exwm-randr-screen-change-hook 'boogs/exwm-change-screen-hook)
        (message "Manual multiscreen handling."))
    (add-hook 'exwm-randr-screen-change-hook 'boogs/exwm-change-screen-hook)
    (message "Automatic multiscreen handling.")))
(add-hook 'exwm-randr-screen-change-hook 'boogs/exwm-change-screen-hook)
;; TODO: Turn the toggle into a global minor mode.
;; Even better: Use autorandr_launcher (https://github.com/phillipberndt/autorandr/issues/210).

;; Don't intercept "C-c" since it frequently used to copy text.
;; EXWM local key bindings won't be availble then.
;; See https://github.com/ch11ng/exwm/wiki#local-key-bindings.

(setq exwm-edit-bind-default-keys nil)
;; REVIEW: The following prevents passing "C-c" to the child window.
;; (when (require 'exwm-edit nil 'noerror)
;;   (exwm-input-set-key (kbd "C-c '") #'exwm-edit--compose)
;;   (exwm-input-set-key (kbd "C-c C-'") #'exwm-edit--compose))

(provide 'init-exwm)
