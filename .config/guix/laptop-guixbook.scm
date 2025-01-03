;; sudo -E guix system reconfigure ~/.config/guix/laptop-guixbook.scm
(define-module (kmonad)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages haskell-apps)
  #:use-module (guix gexp)
  #:export (kmonad-service))

(define (kmonad-shepherd-service config-path)
  ;; Tells shepherd how we want it to create a (single) <shepherd-service>
  ;; for kmonad from a string
  (list (shepherd-service
         (documentation "Run the kmonad daemon (kmonad-daemon).")
         (provision '(kmonad-daemon))
         (requirement '(udev user-processes))
         (start #~(make-forkexec-constructor
                   (list #$(file-append kmonad "/bin/kmonad")
                         #$config-path)))
         (stop #~(make-kill-destructor)))))

(define kmonad-service-type
  ;; Extend the shepherd root into a new type of service that takes a single string
  (service-type
   (name 'kmonad)
   (description "Run the kmonad service type.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             kmonad-shepherd-service)))))

(define (kmonad-service config-path)
  ;; Create a service from our service type, which takes a single parameter
  (service kmonad-service-type config-path))

(use-modules (gnu)
             (gnu system)
             (gnu packages)
             (gnu bootloader)
             (gnu bootloader grub)
             (gnu packages admin)
             (gnu packages cups)
             (gnu packages bash)
             (gnu packages fonts)
             (gnu packages haskell-apps)
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
             (gnu services base)
             (gnu services databases)
             (gnu services desktop)
             (gnu services docker)
             (gnu services mcron)
             (gnu services networking)
             (gnu services syncthing)
             (gnu services virtualization)
             (gnu services xorg)
             (guix gexp)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             (kmonad)
             ;; import nonfree linux module
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (srfi srfi-1)
             (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 popen))

(use-service-modules
 cups
 databases
 desktop
 networking
 ssh
 sddm
 syncthing
 virtualization
 xorg)

(use-package-modules
 databases
 geo
 vpn
 virtualization)

(define rsync-backup-job
  ;; Run 'backup-job' at 7pm every day.
  #~(job '(next-hour '(19))
         (lambda ()
           (system "rsync -avLx --exclude 'node_modules' --progress /home/boogs/repos /media/home/")
           (system "rsync -avLx --exclude 'node_modules' --progress /home/boogs/projects /media/home/")
           (system "rsync -avx --progress /home/boogs/dotfiles /media/home/")
           (system "rsync -avx --progress /home/boogs/common-lisp /media/home/")
           (system "rsync -avx --progress /home/boogs/.gnupg /media/home/")
           (system "rsync -avx --progress /home/boogs/.password-store /media/home")
           (system "rsync -avx --progress /home/boogs/Pictures /media/home"))
         "backup"))

(define btrfs-snapshot-job
  ;; Run 'btrfs-snapshots' at 8PM every day.
  #~(job '(next-hour '(20))
         (lambda ()
           (let ((timestamp (strftime "%F_%R:%S" (localtime (current-time)))))
             (system (string-append "btrfs subvolume snapshot -r /media/personal /media/.snapshots/personal/" timestamp))
             (system (string-append "btrfs subvolume snapshot -r /media/home /media/.snapshots/home/" timestamp)))
           "snapshot")))

(define garbage-collector-job
  ;; Collect garbage 5 minutes after midnight every day.
  ;; The job's action is a shell command.
  #~(job "5 0 * * *" ; Vixie cron syntax
         "guix gc -F 1G"))

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
                (comment "boogs")
                (group "users")
                (supplementary-groups '("wheel" "netdev" "audio" "kvm" "libvirt" "lp" "video"
                                        "input" "docker" "postgres"))
                (home-directory "/home/boogs"))
               %base-user-accounts))
 (packages
  (append
   (list
    (specification->package "bluez")
    (specification->package "bluez-alsa")
    (specification->package "btrfs-progs")
    (specification->package "pulseaudio")
    (specification->package "emacs-exwm-no-x-toolkit")
    (specification->package "glibc")
    (specification->package "gnome")
    (specification->package "grub")
    (specification->package "kmonad")
    (specification->package "rsync")
    (specification->package "st")
    (specification->package "wayland")
    (specification->package "wireguard-tools")
    (specification->package "wofi")
    (specification->package "xorg-server-xwayland"))
   %base-packages))
 (services
  (append
   (list
    ;; Copy current config to /etc/config.scm
    (simple-service 'config-file etc-service-type
                    `(("config.scm" ,this-file)))
    (simple-service 'my-cron-jobs
                    mcron-service-type
                    (list
                     rsync-backup-job
                     btrfs-snapshot-job
                     garbage-collector-job))
    (service bluetooth-service-type
             (bluetooth-configuration
              (auto-enable? #t)))
    (kmonad-service "/home/boogs/dotfiles/x2100.kbd")
    (service gnome-desktop-service-type)
    (service openssh-service-type)
    (service cups-service-type
             (cups-configuration
              (web-interface? #t)
              (extensions
               (list cups-filters epson-inkjet-printer-escpr hplip-minimal))))
    (service containerd-service-type)
    (service docker-service-type)
    (service postgresql-service-type
             (postgresql-configuration
              (extension-packages (list postgis))))
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
    (service redis-service-type)
    (service syncthing-service-type
             (syncthing-configuration
              (user "boogs")))
    (set-xorg-configuration
     (xorg-configuration
      (keyboard-layout keyboard-layout))))))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)
   (theme (grub-theme
           (image (local-file "background.png"))
           (gfxmode '("3000x2000" "auto"))))
   (timeout 2)))
 (mapped-devices
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
          (check? #f)
          (needed-for-boot? #t))
         %base-file-systems)))
