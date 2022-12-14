(use-modules (gnu)
             (gnu system)
             (gnu packages)
             (gnu bootloader)
             (gnu bootloader grub)
             (gnu packages admin)
             (gnu packages bash)
             (gnu packages fonts)
             (gnu packages linux)
             (gnu packages wm)
             (gnu system accounts)
             (gnu system file-systems)
             (gnu system keyboard)
             (gnu system mapped-devices)
             (gnu system nss)
             (gnu system pam)
             (gnu system shadow)
             (gnu services)
             (gnu services xorg)
             (gnu services base)
             (gnu services desktop)
             (gnu services databases)
             (gnu services desktop)
             (gnu services docker)
             (gnu services virtualization)
             (guix gexp)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             ;; import nonfree linux module
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (srfi srfi-1)
             (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 popen))
(use-service-modules databases desktop networking ssh sddm virtualization xorg)
(use-package-modules databases
                     geo
                     vpn
                     virtualization)

(define this-file
  (local-file (basename (assoc-ref (current-source-location) 'filename))
              "config.scm"))

(define linux-guixbook
  (package
    (inherit linux)
    (name "linux-guixbook")
    (native-inputs
      `(("kconfig" ,(local-file "./kernel-guixbook.conf"))
        ,@(alist-delete "kconfig" (package-native-inputs linux))))))

(operating-system
  (locale "en_US.utf8")
  (host-name "guixbook")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"
                                    #:options
                                    '("ctrl:nocaps")))
  (kernel linux)
  ;; (kernel-loadable-modules (list wireguard-linux-compat))
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (users (cons* (user-account
                  (name "boogs")
                  (comment "Bugi Idris")
                  (group "users")
                  (supplementary-groups '("wheel" "netdev" "audio" "kvm" "libvirt" "lp" "video"
                                          "docker" "postgres"))
                  (home-directory "/home/boogs"))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "bluez")
            (specification->package "bluez-alsa")
            (specification->package "pulseaudio")
            (specification->package "emacs-exwm-no-x-toolkit")
            (specification->package "glibc")
            (specification->package "grub")
            (specification->package "nss-certs")
            (specification->package "st")
            (specification->package "wireguard-tools")
            (specification->package "wofi"))
      %base-packages))
  (services
    (append
      (list
        ;; Copy current config to /etc/config.scm
        (simple-service 'config-file etc-service-type
                        `(("config.scm" ,this-file)))
        (bluetooth-service #:auto-enable? #t)
        (service gnome-desktop-service-type)
        (service openssh-service-type)
        (service docker-service-type)
        (postgresql-service #:extension-packages (list postgis))
        ;; (simple-service 'wireguard-module
        ;;                 kernel-module-loader-service-type
        ;;                 '("wireguard"))
        (service libvirt-service-type
                 (libvirt-configuration
                  (unix-sock-group "libvirt")
                  (tls-port "16555")))
        (service virtlog-service-type
                 (virtlog-configuration
                  (max-clients 1000)))
        (set-xorg-configuration
          (xorg-configuration
            (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid "bd375e35-0c57-45b6-9dba-a0f35cbc1987"))
            (target "cryptroot")
            (type luks-device-mapping))
          (mapped-device
            (source
              (uuid "4c06c72d-c6cb-4482-8ab5-0015523ca977"))
            (target "cryptmedia")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "0848-C99A" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
           (file-system
             (mount-point "/media")
             (device "/dev/mapper/cryptmedia")
             (type "btrfs")
             (create-mount-point? #t)
             (dependencies mapped-devices))
           (file-system
             (mount-point "/tmp")
             (device "none")
             (type "tmpfs")
             (check? #f))
           %base-file-systems)))
