;; ThinkPad T70
;; sudo -E guix system -L ~/.config/guix/system reconfigure ~/.config/guix/system/ra.scm
(define-module (ra)
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

(define linux-ra
  (package
   (inherit linux-6.6)
   (name "linux-ra")
   (native-inputs
    `(("kconfig" ,(local-file "./kernel.conf"))
      ,@(alist-delete "kconfig" (package-native-inputs linux-6.6))))))

(define %ra/mapped-devices
  (list (mapped-device
         (source
          (uuid "3c338bcb-e8ca-4767-aeb5-27e1b9b4fcdf"))
         (target "cryptroot")
         (type luks-device-mapping))))

(define-public %ra/bootloader-configuration
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets '("/dev/sda"))
   (theme (grub-theme
          (image (local-file "grub-theme/aot.png"))))
   (timeout 3)))

(define-public %boogs/ra
  (operating-system
   (inherit %boogs/os)
   (host-name "ra")

   (bootloader %ra/bootloader-configuration)

   (kernel linux-ra)
   (initrd microcode-initrd)
   (firmware (append (list linux-firmware)
                     %boogs/firmware))

   (packages (cons* intel-media-driver/nonfree
                    %boogs/packages))
   (services (cons*
              (simple-service 'config-file etc-service-type
                              `(("config.scm" ,this-file)))
              %boogs/services))

   (mapped-devices %ra/mapped-devices)
   (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "btrfs")
                         (dependencies %ra/mapped-devices))
                        %boogs/file-systems))))

%boogs/ra
