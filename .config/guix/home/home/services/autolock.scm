(define-module (home services autolock)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (home-autolock-service-type))

(define (home-autolock-profile-service config)
  (list xss-lock))

(define (home-autolock-shepherd-service config)
  (list
   (shepherd-service
    (requirement '(dbus))
    (provision '(xss-lock))
    (documentation "Run xss-lock for X screen locking.")
    (start #~(make-forkexec-constructor '("xss-lock" "--" "slock")))
    (stop #~(make-kill-destructor)))))

(define home-autolock-service-type
  (service-type (name 'home-autolock)
                (description "A service for launching autolock.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-autolock-profile-service)
                       (service-extension
                        home-shepherd-service-type
                        home-autolock-shepherd-service)))
                (default-value #f)))
