(define-module (keyboard)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages haskell-apps)
  #:use-module (guix gexp)
  #:re-export (kmonad))

(define (kmonad-shepherd-service config-path)
  "Tells shepherd how we want it to create a (single)
<shepherd-service> for kmonad from a string."
  (list (shepherd-service
         (documentation "Run the kmonad daemon (kmonad-daemon).")
         (provision '(kmonad-daemon))
         (requirement '(udev user-processes))
         (start #~(make-forkexec-constructor
                   (list #$(file-append kmonad "/bin/kmonad")
                         #$config-path)))
         (stop #~(make-kill-destructor)))))

;;; Extend the shepherd root into a new type of service that takes a
;;; single string.
(define-public kmonad-service-type
  (service-type
   (name 'kmonad)
   (description "Run the kmonad service type.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             kmonad-shepherd-service)))))

(define-public (kmonad-service config-path)
  "Create a service from our service type, which takes a single
parameter."
  (service kmonad-service-type config-path))
