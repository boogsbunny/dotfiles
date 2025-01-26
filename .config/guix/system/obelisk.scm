;; ThinkPad X210
;; sudo -E guix system -L ~/.config/guix/system reconfigure ~/.config/guix/system/obelisk.scm
(define-module (obelisk)
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

(use-service-modules
 docker)

(define this-file
  (local-file
   (basename (assoc-ref (current-source-location) 'filename))
   "config.scm"))

(define linux-obelisk
  (package
   (inherit linux-6.6)
   (name "linux-obelisk")
   (native-inputs
    `(("kconfig" ,(local-file "./kernel.conf"))
      ,@(alist-delete "kconfig" (package-native-inputs linux-6.6))))))

(define %obelisk/mapped-devices
  (list (mapped-device
         (source
          (uuid "bd375e35-0c57-45b6-9dba-a0f35cbc1987"))
         (target "cryptroot")
         (type luks-device-mapping))
        (mapped-device
         (source
          (uuid "15f045ca-3d0f-4866-8d14-2d4ac7ece415"))
         (target "cryptmedia")
         (type luks-device-mapping))))

(define-public %boogs/obelisk
  (operating-system
   (inherit %boogs/os)
   (host-name "obelisk")

   (kernel linux-obelisk)
   (initrd microcode-initrd)
   (firmware (append (list iwlwifi-firmware)
                     %boogs/firmware))

   (packages (cons* intel-media-driver/nonfree
                    %boogs/packages))
   (services (cons*
              (simple-service 'config-file etc-service-type
                              `(("config.scm" ,this-file)))
              (service oci-container-service-type
                       (list
                        (oci-container-configuration
                         (image "jellyfin/jellyfin")
                         (provision "jellyfin")
                         (network "host")
                         (ports '(("8096" . "8096")))
                         (volumes
                          '("jellyfin-config:/config"
                            "jellyfin-cache:/cache"
                            "/media/personal/entertainment:/media")))))
              %boogs/services))

   (mapped-devices %obelisk/mapped-devices)
   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "0848-C99A" 'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies %obelisk/mapped-devices))
                        (file-system
                         (mount-point "/media")
                         (device "/dev/mapper/cryptmedia")
                         (type "btrfs")
                         (create-mount-point? #t)
                         (dependencies %obelisk/mapped-devices))
                        %boogs/file-systems))))

%boogs/obelisk
