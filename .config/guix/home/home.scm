;; guix home reconfigure -L ~/.config/guix/home home.scm
(define-module (home)
  #:use-module (gnu home)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu services)
  #:use-module (home services udiskie)
  #:use-module (home services pipewire))

(home-environment
 (services (list (service home-dbus-service-type)
                 (service home-udiskie-service-type)
                 (service home-pipewire-service-type)
                 (service home-redshift-service-type
                          (home-redshift-configuration
                           (daytime-temperature 3500)
                           (nighttime-temperature 2000)
                           (location-provider 'manual)
                           (latitude 38.87)
                           (longitude -77.42))))))
