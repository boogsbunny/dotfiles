;; guix system -L ~/.config/guix/system image -r guix.img ~/.config/guix/system/usb.scm
;;
;; then write it to the usb:
;;
;; sudo dd if=/path/to/your-image.img of=/dev/sdX bs=4M status=progress conv=fsync
;; sync
(define-module (usb)
  #:use-module (srfi srfi-1)
  #:use-module (default)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu))

(use-service-modules
 databases
 desktop
 syncthing
 xorg)

(use-package-modules bootloaders)

(define-public %usb/services
  (cons*
   (remove
    (lambda (service)
      (member (service-kind service)
              (list bluetooth-service-type
                    cups-pk-helper-service-type
                    geoclue-service-type
                    gdm-service-type
                    gnome-desktop-service-type
                    plasma-desktop-service-type
                    redis-service-type
                    syncthing-service-type)))
    %boogs/services)))

(operating-system
  (inherit %boogs/os)

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sdb"))))
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (append (list linux-firmware)
                    %boogs/firmware))

  (file-systems (cons* (file-system
                         (device (file-system-label "guix"))
                         (mount-point "/")
                         (type "ext4"))
                       (file-system
                         (device "none")
                         (mount-point "/tmp")
                         (type "tmpfs")
                         (check? #f))
                       %base-file-systems))
  (services %usb/services))
