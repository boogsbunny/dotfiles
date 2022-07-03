;; To get "sudo" to work in SLY.
(let ((askpass (format nil "~a/.local/bin/emacs-askpass" (uiop:getenv "HOME"))))
  (when (uiop:file-exists-p askpass)
    (setf (uiop:getenv "SUDO_ASKPASS") askpass)))
