(define-module (home services cron)
  #:use-module (ice-9 popen)
  #:use-module (rnrs io ports)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (home services utils)
  #:use-module (guix gexp))

(define check-mail
  #~(let ((check-gpg #$gpg-utils))
      (lambda ()
        (when (check-gpg)
          (system* "notmuch" "new")))))

(define-public email-job
  #~(job '(next-minute (range 0 60 10))
         #$check-mail
         "mail"))
