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
             (gnu services desktop)
             (guix gexp)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             (srfi srfi-1)
             (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 popen))
(use-service-modules desktop networking ssh xorg)

(define this-file
  (local-file (basename (assoc-ref (current-source-location) 'filename))
              "config.scm"))

(define-public linux-stable
               (let* ((version "5.4.3"))
                 (package
                   (inherit linux-libre)
                   (name "linux-stable")
                   (version version)
                   (source (origin
                             (method url-fetch)
                             (uri (string-append
                                    "https://cdn.kernel.org/pub/linux/kernel/v5.x/"
                                    "linux-" version ".tar.xz"))
                             (sha256
                               (base32
                                 "0lgfg31pgvdhkh9y4y4yh075mlk3qa6npxp7n19yxcg168pnhcb7"))))
                   (synopsis "Linux kernel: current stable release")
                   (description "Linux is a kernel")
                   (license license:gpl2)
                   (home-page "http://kernel.org/"))))

(define linux-firmware-version "20191215")
(define (linux-firmware-nonfree version)
  (origin
    (method url-fetch)
    (uri (string-append
           "https://mirrors.edge.kernel.org/pub/linux/kernel/firmware/"
           "linux-firmware-" linux-firmware-version ".tar.xz"))
    (sha256
      (base32
        "0mmfq2an2d58209s4pl5jnlsig285bw8i55vxp1z5h4plzs8y1n2"))))

(define-public iwlwifi-firmware-nonfree
               (package
                 (name "iwlwifi-firmware-nonfree")
                 (version linux-firmware-version)
                 (source (linux-firmware-nonfree version))
                 (build-system trivial-build-system)
                 (arguments
                   `(#:modules ((guix build utils))
                     #:builder (begin
                                 (use-modules (guix build utils))
                                 (let ((source (assoc-ref %build-inputs "source"))
                                       (fw-dir (string-append %output "/lib/firmware/")))
                                   (mkdir-p fw-dir)
                                   (for-each (lambda (file)
                                               (copy-file file
                                                          (string-append fw-dir (basename file))))
                                             (find-files source
                                                         "iwlwifi-.*\\.ucode$|LICENSE\\.iwlwifi_firmware$"))
                                   #t))))

                 (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
                 (synopsis "Non-free firmware for Intel wifi chips")
                 (description "Non-free iwlwifi firmware")
                 (license (license:non-copyleft
                            "https://git.kernel.org/cgit/linux/kernel/git/firmware/linux-firmware.git/tree/LICENCE.iwlwifi_firmware?id=HEAD"))))

(operating-system
  (locale "en_US.utf8")
  (host-name "guixbook")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"
                                    #:options
                                    '("caps:ctrl_modifier")))

  (firmware (append (list iwlwifi-firmware-nonfree)
                    %base-firmware))

  (users (cons* (user-account
                  (name "boogs")
                  (comment "Bugi Idris")
                  (group "users")
                  (supplementary-groups '("wheel" "netdev" "audio" "lp" "video"))
                  ;; default to name
                  (home-directory "/home/boogs"))
                %base-user-accounts))

  (packages
    (append
      (list (specification->package "dmenu")
            (specification->package "emacs")
            (specification->package "emacs-desktop-environment")
            (specification->package "emacs-exwm")
            (specification->package "font-dejavu")
            (specification->package "font-gnu-unifont")
            (specification->package "glibc")
            (specification->package "grub")
            (specification->package "htop")
            (specification->package "i3status")
            (specification->package "i3-wm")
            (specification->package "nss-certs")
            (specification->package "st")
            (specification->package "sway")
            (specification->package "swaylock")
            (specification->package "waybar")
            (specification->package "wofi"))
      %base-packages))

  (services
    (append
      (list
        ;; Copy current config to /etc/config.scm
        (simple-service 'config-file etc-service-type
                        `(("config.scm" ,this-file)))
        (service gnome-desktop-service-type)
        (service openssh-service-type)
        (set-xorg-configuration
          (xorg-configuration
            (keyboard-layout keyboard-layout))))
      %desktop-services))

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/nvme0n1")
                (keyboard-layout keyboard-layout)))
  (swap-devices (list "/dev/nvme0n1p2"))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid "ff24d4d0-8a81-413a-b7e1-620519b049dc"))
            (target "crypt")
            (type luks-device-mapping))))

  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/crypt")
                         (type "btrfs")
                         (options "subvol=root")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot")
                         (device "/dev/mapper/crypt")
                         (type "btrfs")
                         (options "subvol=boot")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/gnu")
                         (device "/dev/mapper/crypt")
                         (type "btrfs")
                         (options "subvol=gnu")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/home")
                         (device "/dev/mapper/crypt")
                         (type "btrfs")
                         (options "subvol=home")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/var/log")
                         (device "/dev/mapper/crypt")
                         (type "btrfs")
                         (options "subvol=log")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/data")
                         (device "/dev/mapper/crypt")
                         (type "btrfs")
                         (options "subvol=data")
                         (dependencies mapped-devices))
                       %base-file-systems)))
