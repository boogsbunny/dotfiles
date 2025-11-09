;; guix home reconfigure -L ~/.config/guix/home home.scm
(define-module (home)
  #:use-module (gnu home)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu services)
  #:use-module (home services autolock)
  #:use-module (home services cron)
  #:use-module (home services udiskie)
  #:use-module (home services pipewire))

(home-environment
 (services (list (service home-autolock-service-type)
                 (service home-dbus-service-type)
                 (service home-mcron-service-type
                          (home-mcron-configuration
                           (jobs (list email-job))))
                 (service home-pipewire-service-type)
                 (service home-redshift-service-type
                          (home-redshift-configuration
                           (daytime-temperature 3500)
                           (nighttime-temperature 2000)
                           (location-provider 'manual)
                           (latitude 48.13)
                           (longitude 11.57)))
                 (service home-udiskie-service-type))))
