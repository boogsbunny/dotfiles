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
	     ;; import nonfree linux module
	     (nongnu packages linux)
	     (nongnu system linux-initrd)
	     (srfi srfi-1)
	     (ice-9 pretty-print)
	     (ice-9 match)
	     (ice-9 popen))
(use-service-modules desktop networking ssh xorg)

(define this-file
  (local-file (basename (assoc-ref (current-source-location) 'filename))
	      "config.scm"))

(operating-system
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"
				    #:options
				    '("caps:ctrl_modifier")))
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (host-name "eurobook")
  (users (cons* (user-account
		  (name "boogs")
		  (comment "Bugi Idris")
		  (group "users")
		  (home-directory "/home/boogs")
		  (supplementary-groups
		    '("wheel" "netdev" "audio" "video")))
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
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/sda")
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
	    (source
	      ;; UUID returned by 'cryptsetup luksUUID'
	      (uuid ""))
	    (target "guix")
	    (type luks-device-mapping))))
  (file-systems (cons* (file-system
			 (mount-point "/")
			 (device "/dev/mapper/guix")
			 (type "btrfs")
			 (options "subvol=root,compress=zstd")
			 (dependencies mapped-devices))
		       (file-system
			 (mount-point "/boot")
			 (device "/dev/mapper/guix")
			 (type "btrfs")
			 (options "subvol=boot,compress=zstd")
			 (dependencies mapped-devices))
		       (file-system
			 (mount-point "/gnu")
			 (device "/dev/mapper/guix")
			 (type "btrfs")
			 (options "subvol=gnu,compress=zstd")
			 (dependencies mapped-devices))
		       (file-system
			 (mount-point "/home")
			 (device "/dev/mapper/guix")
			 (type "btrfs")
			 (options "subvol=home,compress=zstd")
			 (dependencies mapped-devices))
		       (file-system
			 (mount-point "/var/log")
			 (device "/dev/mapper/guix")
			 (type "btrfs")
			 (options "subvol=log,compress=zstd")
			 (dependencies mapped-devices))
		       (file-system
			 (mount-point "/data")
			 (device "/dev/mapper/guix")
			 (type "btrfs")
			 (options "subvol=data,compress=zstd")
			 (dependencies mapped-devices))
		       %base-file-systems)))
