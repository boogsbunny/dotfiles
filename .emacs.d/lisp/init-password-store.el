;;--------------------------------------------------------------------
;; password-store
;;--------------------------------------------------------------------

(require 'password-store)

(defun boogs/pass-async-copy (entry)
  (let* ((buf (generate-new-buffer "*pass-async*"))
         (proc (make-process
                :name "pass-async"
                :buffer buf
                :command (list "pass" "show" entry)
                :noquery t)))
    (process-put proc 'buf buf)
    (process-put proc 'entry entry)
    (set-process-sentinel
     proc
     (lambda (p _)
       (when (eq (process-status p) 'exit)
         (let ((exit (process-exit-status p))
               (buf (process-get p 'buf))
               (entry (process-get p 'entry)))
           (if (= exit 0)
               (with-current-buffer buf
                 (goto-char (point-min))
                 (let ((secret (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
                   (kill-new secret)
                   (message "Copied %s" entry))
                 (kill-buffer buf))
             (with-current-buffer buf
               (display-buffer buf)
               (message "pass failed (%d)" exit)))))))
    proc))

(defun boogs/pass-async-otp-copy (entry)
  (let* ((buf (generate-new-buffer "*pass-async*"))
         (proc (make-process
                :name "pass-otp"
                :buffer buf
                :command (list "pass" "otp" "code" entry)
                :noquery t)))
    (process-put proc 'buf buf)
    (process-put proc 'entry entry)
    (set-process-sentinel
     proc
     (lambda (p _)
       (when (eq (process-status p) 'exit)
         (let ((exit (process-exit-status p))
               (buf (process-get p 'buf))
               (entry (process-get p 'entry)))
           (if (= exit 0)
               (with-current-buffer buf
                 (let ((code (string-trim (buffer-string))))
                   (kill-new code)
                   (message "OTP copied: %s" entry))
                 (kill-buffer buf))
             (with-current-buffer buf
               (display-buffer buf)
               (message "OTP failed (%d)" exit)))))))
    proc))

(defun boogs/password-store-copy-async ()
  (interactive)
  (boogs/pass-async-copy
   (password-store--completing-read)))

(defun boogs/password-store-otp-copy-async ()
  (interactive)
  (boogs/pass-async-otp-copy
   (password-store--completing-read)))

(provide 'init-password-store)
