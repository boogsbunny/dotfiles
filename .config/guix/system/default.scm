(define-module (default)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system nss)
  #:use-module (guix packages)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages syncthing)
  #:use-module (backup)
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
 suckless
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
         font-aporetic
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
         slock
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
        (service screen-locker-service-type
                 (screen-locker-configuration
                  (name "slock")
                  (program (file-append slock "/bin/slock"))))
        (service syncthing-service-type
                 (syncthing-configuration
                  (user "boogs")))
        (service thermald-service-type
                 (thermald-configuration
                  (adaptive? #t)))
        (service tlp-service-type
                 (tlp-configuration
                  ;; Force powersave governor always
                  (cpu-scaling-governor-on-ac (list "powersave"))
                  (cpu-scaling-governor-on-bat (list "powersave"))

                  ;; Limit CPU performance
                  (cpu-max-perf-on-ac 60) ;; Limit to 60% on AC
                  (cpu-max-perf-on-bat 30) ;; Limit to 30% on battery
                  (cpu-boost-on-ac? #f)    ;; Disable turbo boost
                  (cpu-boost-on-bat? #f)

                  ;; Aggressive scheduler power saving
                  (sched-powersave-on-ac? #t)
                  (sched-powersave-on-bat? #t)

                  ;; Maximize power management
                  (runtime-pm-on-ac "auto")
                  (runtime-pm-on-bat "auto")
                  (runtime-pm-all? #t)

                  ;; Aggressive disk power saving
                  (disk-idle-secs-on-ac 5)
                  (disk-idle-secs-on-bat 2)
                  (disk-spindown-timeout-on-ac (list "120"))
                  (disk-spindown-timeout-on-bat (list "60"))

                  ;; PCIe power saving
                  (pcie-aspm-on-ac "powersave")
                  (pcie-aspm-on-bat "powersave")

                  ;; Other power savings
                  (wifi-pwr-on-ac? #t)
                  (wifi-pwr-on-bat? #t)
                  (nmi-watchdog? #f)

                  ;; Misc
                  (usb-autosuspend? #f)))
        (service virtlog-service-type
                 (virtlog-configuration
                  (max-clients 1000)))))

(define %desktop-environment
  (list (service plasma-desktop-service-type)))

(define %development-services
  (list (service containerd-service-type)
        (service docker-service-type)
        (service postgresql-service-type
                 (postgresql-configuration
                  (postgresql postgresql-16)
                  ;; (extension-packages (list postgis))
                  (config-file
                   (postgresql-config-file
                    (log-destination "stderr")
                    (hba-file
                     (plain-file "pg_hba.conf"
                                 "
local	all	all			trust
host	all	all	127.0.0.1/32 	md5
host	all	all	::1/128 	md5"))))))
        (service postgresql-role-service-type
                 (postgresql-role-configuration
                  (roles (list (postgresql-role
                                (name "boogs")
                                (create-database? #t))
                               (postgresql-role
                                (name "postgres")
                                (permissions
                                 '(bypassrls
                                   createdb
                                   createrole
                                   login
                                   replication
                                   superuser))
                                (create-database? #t))))))
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
   (supplementary-groups '("audio"
                           "docker"
                           "input"
                           "kvm"
                           "libvirt"
                           "lp"
                           "netdev"
                           "postgres"
                           "realtime"
                           "video"
                           "wheel"))
   (home-directory "/home/boogs")))

(define-public %boogs/file-systems
  (cons* (file-system
          (mount-point "/tmp")
          (device "none")
          (type "tmpfs")
          (check? #f)
          (needed-for-boot? #t))
         %base-file-systems))

(define-public %boogs/bootloader-configuration
  (bootloader-configuration
   (bootloader grub-efi-removable-bootloader)
   (targets '("/boot/efi"))
   (theme (grub-theme
          (image (local-file "grub-theme/aot.png"))))
   (timeout 3)))

(define-public %boogs/os
  (operating-system
   (host-name "guixbook")
   (timezone "America/New_York")
   (locale "en_US.utf8")

   (bootloader %boogs/bootloader-configuration)

   (firmware %boogs/firmware)

   (file-systems (cons* %boogs/file-systems))

   (groups (cons (user-group (system? #t) (name "realtime"))
                 %base-groups))

   (users (cons* %boogs/user
                 %base-user-accounts))

   (packages %boogs/packages)
   (services %boogs/services)))

%boogs/os
