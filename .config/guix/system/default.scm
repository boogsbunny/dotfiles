(define-module (default)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system nss)
  #:use-module (guix packages)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages syncthing)
  #:use-module (backup)
  #:use-module (desktop)
  #:use-module (keyboard))

(use-service-modules
 cups
 databases
 desktop
 docker
 networking
 pm
 ssh
 syncthing
 virtualization
 vpn
 xorg)

(use-package-modules
 audio
 databases
 emacs-xyz
 fonts
 geo
 gnome
 linux
 rsync
 vim
 vpn
 virtualization)

(define %unwanted-packages
  '("mg"
    "nano"
    "wireless-tools"))

(define-public %boogs/packages
  (cons* bluez
         bluez-alsa
         brightnessctl
         btrfs-progs
         emacs-exwm-no-x-toolkit
         font-awesome
         font-dejavu
         font-fira-code
         font-fira-mono
         font-iosevka
         font-iosevka-comfy
         font-iosevka-term
         font-google-noto
         font-openmoji
         fontconfig
         gnome-keyring
         gvfs
         kmonad
         neovim
         pipewire
         rsync
         syncthing
         tlp
         wireplumber
         (fold (lambda (package l)
                 (delete package l))
               %base-packages
               %unwanted-packages)))

(define %base-services
  (list (service bluetooth-service-type
                 (bluetooth-configuration
                  (auto-enable? #t)))
        (service cups-service-type
                 (cups-configuration
                  (web-interface? #t)
                  (extensions
                   (list cups-filters
                         epson-inkjet-printer-escpr
                         hplip-minimal))))
        (service libvirt-service-type
                 (libvirt-configuration
                  (unix-sock-group "libvirt")
                  (tls-port "16555")))
        (service openssh-service-type)
        (service syncthing-service-type
                 (syncthing-configuration
                  (user "boogs")))
        (service thermald-service-type)
        (service tlp-service-type
                 (tlp-configuration
                  (cpu-scaling-governor-on-ac (list "performance"))
                  (sched-powersave-on-bat? #t)))
        (service virtlog-service-type
                 (virtlog-configuration
                  (max-clients 1000)))))

(define %desktop-environment
  (list (service gnome-desktop-service-type
                 (gnome-desktop-configuration
                  (gnome %boogs/gnome-minimal)))))

(define %development-services
  (list (service containerd-service-type)
        (service docker-service-type)
        (service postgresql-service-type
                 (postgresql-configuration
                  (postgresql postgresql-16)
                  (extension-packages (list postgis))))
        (service redis-service-type)))

(define %desktop-services-customization
  (modify-services %desktop-services
                   (console-font-service-type
                    config =>
                    (map (lambda (tty)
                           (cons tty
                                 (file-append
                                  font-terminus
                                  "/share/consolefonts/ter-132n")))
                         '("tty1"
                           "tty2"
                           "tty3"
                           "tty4"
                           "tty5"
                           "tty6")))))

(define-public %boogs/services
  (append (list (kmonad-service "/home/boogs/.config/kmonad/thinkbook.kbd"))
          %backup-services
          %base-services
          %desktop-environment
          %development-services
          %desktop-services-customization))

(define-public %boogs/firmware
  %base-firmware)

(define-public %boogs/user
  (user-account
   (name "boogs")
   (group "users")
   (supplementary-groups '("wheel"
                           "netdev"
                           "audio"
                           "kvm"
                           "libvirt"
                           "lp"
                           "video"
                           "input"
                           "docker"
                           "postgres"))
   (home-directory "/home/boogs")))

(define-public %boogs/file-systems
  (cons* (file-system
          (mount-point "/tmp")
          (device "none")
          (type "tmpfs")
          (check? #f)
          (needed-for-boot? #t))
         %base-file-systems))

(define-public %boogs/os
  (operating-system
   (host-name "guixbook")
   (timezone "America/New_York")
   (locale "en_US.utf8")

   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-removable-bootloader)
     (targets '("/boot/efi"))
     (theme (grub-theme
             (image (local-file "grub-theme/aot.png"))))
     (timeout 3)))

   (firmware %boogs/firmware)

   (file-systems (cons* %boogs/file-systems))

   (users (cons* %boogs/user
                 %base-user-accounts))

   (packages %boogs/packages)
   (services %boogs/services)))

%boogs/os
