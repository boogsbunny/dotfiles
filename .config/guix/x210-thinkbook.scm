;; sudo -E guix system reconfigure ~/.config/guix/x210-thinkbook.scm
(define-module (kmonad)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages haskell-apps)
  #:use-module (guix gexp)
  #:export (kmonad-service))

(define (kmonad-shepherd-service config-path)
  "Tells shepherd how we want it to create a (single)
<shepherd-service> for kmonad from a string."
  (list (shepherd-service
         (documentation "Run the kmonad daemon (kmonad-daemon).")
         (provision '(kmonad-daemon))
         (requirement '(udev user-processes))
         (start #~(make-forkexec-constructor
                   (list #$(file-append kmonad "/bin/kmonad")
                         #$config-path)))
         (stop #~(make-kill-destructor)))))

;;; Extend the shepherd root into a new type of service that takes a
;;; single string.
(define kmonad-service-type
  (service-type
   (name 'kmonad)
   (description "Run the kmonad service type.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             kmonad-shepherd-service)))))

(define (kmonad-service config-path)
  "Create a service from our service type, which takes a single
parameter."
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
             (gnu services pm)
             (gnu services syncthing)
             (gnu services virtualization)
             (gnu services vpn)
             (gnu services xorg)
             (guix gexp)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             (guix utils)
             (kmonad)
             ;; import nonfree linux module
             (nongnu packages linux)
             (nongnu packages video)
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
 pm
 ssh
 sddm
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
 gnome-xyz
 linux
 rsync
 vim
 vpn
 virtualization)

(define-public %backup-paths
  '(("repos" . "-avLx --exclude 'node_modules'")
    ("projects" . "-avLx --exclude 'node_modules'")
    ("dotfiles" . "-avx")
    ("common-lisp" . "-avx")
    (".gnupg" . "-avx")
    (".password-store" . "-avx")
    ("grapheneboogs" . "-avx")
    ("Pictures" . "-avx")))

(define rsync-backup-job
  #~(begin
      (define home-directory
        (or (getenv "HOME")
            (error "HOME environment variable not set")))

      (define media-directory
        (if (file-exists? "/media")
            "/media/home/"
            (string-append home-directory "/media/home/")))

      (define (make-rsync-command path opts)
        (format #f "rsync ~a --progress ~a/~a ~a"
                opts home-directory path media-directory))

      (job '(next-hour '(19))
           (lambda ()
             (for-each (lambda (path-opts)
                         (system (make-rsync-command
                                 (car path-opts)
                                 (cdr path-opts))))
                       '#$%backup-paths))
           "backup")))

(define btrfs-snapshot-job
  #~(begin
      (define (make-snapshot-command subvol timestamp)
        (format #f
                "btrfs subvolume snapshot -r /media/~a ~
                 /media/.snapshots/~a/~a"
                subvol
                subvol
                timestamp))

      (job '(next-hour '(20))
           (lambda ()
             (let ((timestamp (strftime "%F_%R:%S"
                                      (localtime (current-time)))))
               (for-each
                (lambda (subvol)
                  (system (make-snapshot-command subvol timestamp)))
                '("personal" "home"))))
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
   (inherit linux-6.6)
   (name "linux-guixbook")
   (native-inputs
    `(("kconfig" ,(local-file "./kernel-thinkbook.conf"))
      ,@(alist-delete "kconfig" (package-native-inputs linux-6.6))))))

(operating-system
 (locale "en_US.utf8")
 (host-name "guixbook")
 (timezone "America/New_York")
 (keyboard-layout (keyboard-layout "us"
                                   #:options '("ctrl:nocaps")))
 (kernel linux-guixbook)
 (initrd microcode-initrd)
 (firmware (cons* iwlwifi-firmware
                  %base-firmware))
 (users (cons* (user-account
                (name "boogs")
                (comment "boogs")
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
                (home-directory "/home/boogs"))
               %base-user-accounts))
 (packages
  (cons* bluez
         bluez-alsa
         brightnessctl
         btrfs-progs
         emacs-exwm-no-x-toolkit
         gvfs
         intel-media-driver/nonfree
         kmonad
         neovim
         rsync
         tlp
         vim
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
    (kmonad-service "/home/boogs/.config/kmonad/thinkbook.kbd")
    (service containerd-service-type)
    (service cups-service-type
             (cups-configuration
              (web-interface? #t)
              (extensions
               (list cups-filters
                     epson-inkjet-printer-escpr
                     hplip-minimal))))
    (service docker-service-type)
    (service gnome-desktop-service-type)
    (service plasma-desktop-service-type)
    (service libvirt-service-type
             (libvirt-configuration
              (unix-sock-group "libvirt")
              (tls-port "16555")))
    (service oci-container-service-type
             (list
              (oci-container-configuration
               (image "jellyfin/jellyfin")
               (provision "jellyfin")
               (network "host")
               (ports
                '(("8096" . "8096")))
               (volumes '("jellyfin-config:/config"
                          "jellyfin-cache:/cache"
                          "/media/personal/entertainment:/media")))))
    (service openssh-service-type)
    (service postgresql-service-type
             (postgresql-configuration
              (postgresql postgresql-16)
              (extension-packages (list postgis))))
    (service redis-service-type)
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
              (max-clients 1000)))
    (set-xorg-configuration
     (xorg-configuration
      (keyboard-layout keyboard-layout))))
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
                            "tty6")))
                    (gdm-service-type
                     config =>
                     (gdm-configuration
                      (inherit config)
                      (wayland? #t))))))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-removable-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)
   (theme (grub-theme
           (image (local-file "aot.png"))))
   (timeout 3)))
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
