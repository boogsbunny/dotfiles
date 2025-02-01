(define-module (home services utils)
  #:use-module (ice-9 popen)
  #:use-module (rnrs io ports)
  #:use-module (guix gexp))

(define-public gpg-utils
  (with-imported-modules
   '((ice-9 popen)
     (ice-9 rdelim))
   #~(begin
       (use-modules (ice-9 popen)
                    (rnrs io ports))
       (let ()
         (letrec ((system-to-string
                   (lambda args
                     (let* ((port (apply open-pipe* OPEN_READ args))
                            (str (get-string-all port)))
                       (close-pipe port)
                       str)))

                  (gpg-keyinfo
                   (lambda ()
                     (filter (lambda (info) (string= (car info) "S"))
                             (map (lambda (s) (string-split s #\space))
                                  (string-split
                                   (system-to-string "gpg-connect-agent"
                                                     "keyinfo --list"
                                                     "/bye")
                                   #\newline)))))

                  (gpg-key-cached?
                   (lambda ()
                     (let ((keyinfo (gpg-keyinfo)))
                       (not (null? (filter (lambda (info)
                                             (string= (list-ref info 6) "1"))
                                           keyinfo)))))))
           gpg-key-cached?)))))
