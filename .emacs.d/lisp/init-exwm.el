;;--------------------------------------------------------------------
;; exwm
;;--------------------------------------------------------------------

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; force-kill Emacs before it can run through `kill-emacs-hook'.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;; Looks like there is a bug between Helm and EXWM global keys which leads Helm
;; to display nothing when only `exwm-input-set-key' is used.
;; https://github.com/ch11ng/exwm/issues/816
(defun boogs/exwm-global-set-key (keys command)
  "Bind KEYS to COMMAND.
KEYS is passed to `kbd'."
  (define-key exwm-mode-map (kbd keys) command)
  (global-set-key (kbd keys) command))

;;; Rename buffer to window title.
(defun boogs/exwm-rename-buffer-to-title ()
  (exwm-workspace-rename-buffer exwm-title))

;; rename buffer to class name
(defun boogs/exwm-rename-buffer-to-class-name ()
  (exwm-workspace-rename-buffer exwm-class-name))

(add-hook 'exwm-update-title-hook 'boogs/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;; System tray
(defun boogs/toggle-conky ()
  "Toggle Conky visibility."
  (interactive)
  (let ((conky-pid (shell-command-to-string "pidof dzen2")))
    (if (string= conky-pid "")
        (let ((win (shell-command "conky 2>/dev/null | dzen2 -h \"50\" -p -dock -ta l -fn \"Iosevka Comfy-$STATUSBAR_SIZE\" &")))
          (with-current-buffer (window-buffer win)
            (fundamental-mode)
            (rename-buffer "*Async conky|dzen*"))
          (delete-window win))
      (let ((win "*Async conky|dzen*"))
        (if (get-buffer win)
            (kill-buffer win))
        (shell-command (concat "kill -9 " conky-pid))))))
(exwm-input-set-key (kbd "s-c") #'boogs/toggle-conky)

(require 'exwm-randr)
(exwm-randr-enable)

;; dual monitor
(setq exwm-randr-workspace-output-plist '(0 "DP-2" 1 "eDP-1"))

;; multi-monitor
(defun boogs/multi-monitor-start ()
  (interactive)
  (start-process-shell-command "hz-two" nil "hz-two"))
(exwm-input-set-key (kbd "s-q") #'boogs/multi-monitor-start)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "C-x q") #'exwm-restart)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-W") #'exwm-workspace-switch-create)

;; TODO: reuse existing buffer
(defun boogs/start-or-switch-to-program (program)
  (interactive)
  (let ((buffer-name-regex (format ".*%s.*" program))
        (existing-buffer nil)
        (existing-process nil))
    (dolist (buffer (buffer-list))
      (when (string-match-p buffer-name-regex (buffer-name buffer))
        (setq existing-buffer buffer)))
    (if existing-buffer
        (progn
          (setq existing-process (get-buffer-process existing-buffer))
          (if existing-process
              (progn
                (switch-to-buffer existing-buffer)
                (select-window (display-buffer (process-buffer existing-process))))
            (start-process program nil program)
            (set-process-buffer (get-process program) existing-buffer))))
      (start-process program nil program)))

(exwm-input-set-key (kbd "C-c C-<return>") (lambda ()
                                             (interactive)
                                             (boogs/start-or-switch-to-program "firefox")))

(exwm-input-set-key (kbd "C-x C-<return>") (lambda ()
                                      (interactive)
                                      (boogs/start-or-switch-to-program "alacritty")))

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
  (boogs/exwm-global-set-key "s-b" #'helm-mini)
  (boogs/exwm-global-set-key "\C-x\C-m" #'helm-M-x)
  (boogs/exwm-global-set-key "C-c t" #'helm-tramp)
  (boogs/exwm-global-set-key "C-x b" #'helm-mini)
  (boogs/exwm-global-set-key "s-f" #'helm-find-files)
  (boogs/exwm-global-set-key "C-x f" #'helm-find-files)
  (boogs/exwm-global-set-key "C-x r g" #'helm-ls-git)
  (boogs/exwm-global-set-key "C-x C-d" #'helm-browse-project)
  (boogs/exwm-global-set-key "C-x r p" #'helm-projects-history)
  ;; (push `(,(kbd "s-f") . helm-find-files) exwm-input-global-keys)
  ;; (exwm-input-set-key (kbd "s-F") #'helm-locate)
  (exwm-input-set-key (kbd "s-F") #'find-file-literally)
  ;; (when (fboundp 'boogs/helm-locate-meta)
  ;;   (boogs/exwm-global-set-key "s-F" #'boogs/helm-locate-meta))
  (boogs/exwm-global-set-key "s-g" 'boogs/helm-grep-git-or-ag)
  (boogs/exwm-global-set-key "s-G" 'boogs/helm-grep-git-all-or-ag)
  ;; Launcher
  (boogs/exwm-global-set-key "s-r" 'helm-run-external-command))

(when (require 'evil nil t)
  (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
  (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

(when (require 'perspective nil t)
  (boogs/exwm-global-set-key "s-s" #'persp-switch)
  (exwm-input-set-key (kbd "s-,") #'persp-prev)
  (exwm-input-set-key (kbd "s-.") #'persp-next)
  (exwm-input-set-key (kbd "s-[") #'perspective-exwm-cycle-exwm-buffers-backward)
  (exwm-input-set-key (kbd "s-]") #'perspective-exwm-cycle-exwm-buffers-forward))

;;; Emacs mode shortcuts.
(autoload 'boogs/helm-selector-sly "init-sly")
(if (not (require 'helm-selector nil :noerror))
    (progn
      (exwm-input-set-key (kbd "s-t") (lambda ()
                                        (interactive)
                                        (find-file (car org-agenda-files))))
      (exwm-input-set-key (kbd "s-n") #'eshell)
      (exwm-input-set-key (kbd "s-m") #'notmuch-hello)
      (exwm-input-set-key (kbd "s-e") #'helm-exwm))
  (boogs/exwm-global-set-key "s-t" 'helm-selector-org)
  (boogs/exwm-global-set-key "s-T" 'helm-selector-org-other-window)
  (boogs/exwm-global-set-key "s-<return>" 'boogs/helm-selector-sly)
  (boogs/exwm-global-set-key "C-x C-<tab>" #'eshell)
  (boogs/exwm-global-set-key "S-s-<return>" 'boogs/helm-selector-sly-other-window)
  (boogs/exwm-global-set-key "s-m" #'helm-selector-notmuch)
  (boogs/exwm-global-set-key "s-M" #'helm-selector-notmuch-other-window)
  (boogs/exwm-global-set-key "s-n" #'helm-selector-elfeed)
  (boogs/exwm-global-set-key "s-N" #'helm-selector-elfeed-other-window) ; "n" for "news"
  (boogs/exwm-global-set-key "s-e" #'helm-exwm)
  (boogs/exwm-global-set-key "s-v" #'helm-selector-magit)
  (boogs/exwm-global-set-key "s-u" #'helm-switch-shell)
  (boogs/exwm-global-set-key "s-E" #'helm-selector-eww-other-window))

(when (fboundp 'magit-status)
  (if (require 'helm-selector-magit nil :noerror)
      (progn
        (boogs/exwm-global-set-key "s-v" #'helm-selector-magit)
        (exwm-input-set-key (kbd "s-V") #'magit-status))
    (exwm-input-set-key (kbd "s-v") #'magit-status)))

(cond
 ((fboundp 'emms-all)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "S-s-<kp-enter>") #'emms-pause)
  (if (fboundp 'helm-emms)
      (boogs/exwm-global-set-key "s-A" #'helm-emms)
    (exwm-input-set-key (kbd "s-A") #'emms))))

(when (fboundp 'helm-pass)
  (boogs/exwm-global-set-key "s-p" #'helm-pass))

(autoload 'boogs/slime-to-repl "lisp")
(autoload 'boogs/helm-selector-sly-non-boogs "init-sly")
(exwm-input-set-key (kbd "C-<backspace>") #'boogs/helm-selector-sly-non-boogs)
(defun boogs/repl-switcher ()
  "Switch between Geiser and SLIME REPLs."
  (interactive)
  ;; TODO: Apparently, S-s-<backspace> is not recognized.
  (pcase
      (completing-read "Lisp: " '(cider geiser slime sly racket))
    ("cider"
     (boogs/exwm-global-set-key "s-<backspace>" 'helm-selector-cider)
     (boogs/exwm-global-set-key "M-s-<backspace>" 'helm-selector-cider-other-window))
    ("geiser"
     (autoload 'helm-geiser-repl-switch "init-scheme")
     (boogs/exwm-global-set-key "s-<backspace>" 'helm-selector-geiser)
     (boogs/exwm-global-set-key "M-s-<backspace>" 'helm-selector-geiser-other-window))
    ("slime"
     (boogs/exwm-global-set-key "s-<backspace>" 'helm-selector-slime)
     (boogs/exwm-global-set-key "M-s-<backspace>" 'helm-selector-slime-other-window))
    ("sly"
     (boogs/exwm-global-set-key "s-<backspace>" 'helm-selector-sly)
     (boogs/exwm-global-set-key "<M-s-backspace>" 'helm-selector-sly-other-window))
    ("racket"
     (boogs/exwm-global-set-key "s-<backspace>" #'racket-repl))))
(exwm-input-set-key (kbd "s-C-<backspace>") #'boogs/repl-switcher)

;; browser
(defun boogs/nyxt-start ()
  (interactive)
  (start-process-shell-command "nyxt" nil "nyxt"))
(exwm-input-set-key (kbd "C-c C-<tab>") #'boogs/nyxt-start)

;;; External application shortcuts.
(defun boogs/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "C-x SPC") #'boogs/exwm-start)

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
(add-to-list 'exwm-manage-configurations '((string= exwm-title "Kingdom Come: Deliverance")
                                           managed t))

;; Gens
(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "Gens") floating nil))

;; Askpass.
(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "lxqt-openssh-askpass")
                                           floating t))

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
