;; ThinkPad X220T
;; sudo -E guix system -L ~/.config/guix/system reconfigure ~/.config/guix/system/slifer.scm
(define-module (slifer)
  #:use-module (srfi srfi-1)
  #:use-module (default)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (nongnu system linux-initrd))

(define this-file
  (local-file
   (basename (assoc-ref (current-source-location) 'filename))
   "config.scm"))

(define linux-slifer
  (package
   (inherit linux-6.6)
   (name "linux-slifer")
   (native-inputs
    `(("kconfig" ,(local-file "./kernel.conf"))
      ,@(alist-delete "kconfig" (package-native-inputs linux-6.6))))))

(define %slifer/mapped-devices
  (list (mapped-device
         (source
          (uuid "a102af1a-5500-4ed1-adfa-49457eb7fbf9"))
         (target "cryptroot")
         (type luks-device-mapping))))

(define-public %slifer/bootloader-configuration
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets '("/dev/sda"))
   (theme (grub-theme
          (image (local-file "grub-theme/aot.png"))))
   (timeout 3)))

(define-public %boogs/slifer
  (operating-system
   (inherit %boogs/os)
   (host-name "slifer")

   (bootloader %slifer/bootloader-configuration)

   (kernel linux-slifer)
   (initrd microcode-initrd)
   (firmware (append (list linux-firmware)
                     %boogs/firmware))

   (packages (cons* intel-media-driver/nonfree
                    %boogs/packages))
   (services (cons*
              (simple-service 'config-file etc-service-type
                              `(("config.scm" ,this-file)))
              %boogs/services))

   (mapped-devices %slifer/mapped-devices)
   (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "btrfs")
                         (dependencies %slifer/mapped-devices))
                        %boogs/file-systems))))

%boogs/slifer
