;;--------------------------------------------------------------------
;; browser
;;--------------------------------------------------------------------

(defgroup boogs-browser nil
  "Browser launching and CPU limiting."
  :group 'applications)

(defcustom boogs/browser-command "firefox"
  "Browser executable to launch."
  :type 'string
  :group 'boogs-browser)

(defcustom boogs/browser-cpu-limit 40
  "CPU limit percentage for the browser."
  :type 'integer
  :group 'boogs-browser)

(defcustom boogs/browser-limit-delay 8
  "Seconds to wait before applying cpulimit after launch."
  :type 'number
  :group 'boogs-browser)

(defcustom boogs/browser-class-names '("Firefox" "Nightly")
  "EXWM class names considered browser-like."
  :type '(repeat string)
  :group 'boogs-browser)

(defvar boogs/browser-cpulimit-process nil
  "cpulimit process started by this Emacs config.")

(defvar boogs/browser-cpulimit-target-pid nil
  "PID currently targeted by boogs/browser-cpulimit-process.")

(defun boogs/browser-exwm-buffer-p (buffer)
  "Return non-nil if BUFFER is a browser EXWM buffer."
  (with-current-buffer buffer
    (and (eq major-mode 'exwm-mode)
         (boundp 'exwm-class-name)
         exwm-class-name
         (member (downcase exwm-class-name)
                 (mapcar #'downcase boogs/browser-class-names)))))

(defun boogs/browser-exwm-buffers ()
  "Return browser EXWM buffers."
  (seq-filter #'boogs/browser-exwm-buffer-p (buffer-list)))

(defun boogs/exwm-window-id-for-xprop ()
  "Return current EXWM buffer's X window id in a form xprop accepts."
  (cond
   ((not (and (boundp 'exwm--id) exwm--id))
    nil)

   ((integerp exwm--id)
    (format "0x%x" exwm--id))

   ((stringp exwm--id)
    exwm--id)

   (t
    (message "Unexpected exwm--id type: %S" exwm--id)
    nil)))

(defun boogs/browser-exwm-buffer-window-pid (buffer)
  "Return _NET_WM_PID for browser EXWM BUFFER, or nil."
  (when-let ((xprop (executable-find "xprop")))
    (with-current-buffer buffer
      (when-let ((window-id (boogs/exwm-window-id-for-xprop)))
        (with-temp-buffer
          (when (eq 0 (call-process
                       xprop nil t nil
                       "-id" window-id
                       "_NET_WM_PID"))
            (goto-char (point-min))
            (when (re-search-forward "= \\([0-9]+\\)" nil t)
              (match-string 1))))))))

(defun boogs/browser-main-window-pid ()
  "Return the PID from an existing browser EXWM window."
  (seq-some #'boogs/browser-exwm-buffer-window-pid
            (boogs/browser-exwm-buffers)))

(defun boogs/stop-browser-cpulimit ()
  "Stop cpulimit process started by this Emacs config."
  (when (process-live-p boogs/browser-cpulimit-process)
    (delete-process boogs/browser-cpulimit-process))
  (setq boogs/browser-cpulimit-process nil
        boogs/browser-cpulimit-target-pid nil))

(defun boogs/limit-browser-cpu (&optional attempts)
  "Attach cpulimit to the PID reported by the browser EXWM window."
  (let ((attempts (or attempts 40))
        (cpulimit (executable-find "cpulimit"))
        (pid (boogs/browser-main-window-pid)))
    (cond
     ((not cpulimit)
      (message "cpulimit not found in Emacs exec-path"))

     ((not (executable-find "xprop"))
      (message "xprop not found in Emacs exec-path"))

     ((not pid)
      (if (> attempts 0)
          (run-at-time 0.25 nil #'boogs/limit-browser-cpu (1- attempts))
        (message "Browser EXWM window PID not found; cpulimit not attached")))

     ((and (process-live-p boogs/browser-cpulimit-process)
           (equal pid boogs/browser-cpulimit-target-pid))
      (message "Browser PID %s already limited to %s%%"
               pid boogs/browser-cpu-limit))

     (t
      (boogs/stop-browser-cpulimit)
      (setq boogs/browser-cpulimit-target-pid pid
            boogs/browser-cpulimit-process
            (start-process
             "cpulimit-browser"
             "*cpulimit-browser*"
             cpulimit
             "-p" pid
             "-l" (number-to-string boogs/browser-cpu-limit)))
      (message "Limited browser PID %s to %s%%"
               pid boogs/browser-cpu-limit)))))

(defun boogs/start-browser-fast-then-limit (&optional force-new-window)
  "Start/open browser quickly, then apply cpulimit after EXWM sees the window.
With FORCE-NEW-WINDOW, ask an already-running browser for a new window."
  (interactive)
  (let ((browser (or (executable-find boogs/browser-command)
                     (user-error "Browser not found in Emacs exec-path: %s"
                                 boogs/browser-command)))
        (existing (boogs/browser-exwm-buffers)))
    ;; Temporarily remove limiter so browser startup / IPC is responsive.
    (boogs/stop-browser-cpulimit)

    (if (and force-new-window existing)
        (start-process
         "browser-new-window"
         "*browser*"
         browser
         "--new-window"
         "about:blank")
      (start-process "browser" "*browser*" browser))

    (message "Started/opened browser; limiting CPU in %s seconds"
             boogs/browser-limit-delay)
    (run-at-time boogs/browser-limit-delay nil #'boogs/limit-browser-cpu)))

(defun boogs/exwm-browser-switch-or-start-limited ()
  "Switch to browser if present, otherwise start it and limit CPU later.
Double-tap quickly to force a new browser window."
  (interactive)
  (let* ((now (float-time))
         (delta (- now boogs/switch-to-or-start--last-invocation))
         (candidates (boogs/browser-exwm-buffers)))
    (setq boogs/switch-to-or-start--last-invocation now)
    (cond
     ((< delta boogs/switch-to-or-start-double-tap-threshold)
      (boogs/start-browser-fast-then-limit t))

     (candidates
      (switch-to-buffer (car candidates))
      (boogs/limit-browser-cpu))

     (t
      (boogs/start-browser-fast-then-limit nil)))))

(boogs/exwm-global-set-key
 "s-i"
 #'boogs/exwm-browser-switch-or-start-limited)

(boogs/exwm-global-set-key
 "s-I"
 (lambda ()
   (interactive)
   (boogs/start-browser-fast-then-limit t)))

(provide 'init-exwm-browser)
