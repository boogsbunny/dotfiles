(defun lass-compile-current ()
  (interactive)
  (or (when (and (sly-connected-p)
                 (or (sly-eval '(cl:not (cl:null (cl:find-package :lass))))
                     (and (sly-eval '(cl:not (cl:null (cl:find-package :ql))))
                          (sly-eval '(ql:quickload :lass)))))
        (message "LASS compiled to %s"
                 (sly-eval `(uiop:native-namestring (lass:generate (uiop:parse-native-namestring ,(buffer-file-name)))))))
      (message "LASS compiled. %s"
               (shell-command-to-string (format "lass %s" (shell-quote-argument (buffer-file-name)))))))

(define-derived-mode lass-mode common-lisp-mode
  "LASS" "Mode with auto-compiling for LASS files."
  (add-hook 'after-save-hook 'lass-compile-current nil t))

(add-to-list 'auto-mode-alist '("\\.lass\\'" . lass-mode))

(provide 'patch-lass)
