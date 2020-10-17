;;--------------------------------;
;; Functions
;;--------------------------------;

(defun boogs/call-process-to-string (program &rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file program nil t nil args))))

(provide 'functions)
