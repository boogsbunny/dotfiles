(use-modules (gnu) 
	     (nongnu packages linux)
	     (guix gexp)
	     (guix packages)
	     (guix download)
	     (guix git-download)
	     (guix build-system trivial)
	     ((guix licenses) #:prefix license:)
	     (gnu packages linux)
	     (srfi srfi-1))
(use-service-modules desktop networking ssh xorg)

;;(define-module (boogs linux-custom)
;;               #:use-module (guix gexp)
;;               #:use-module (guix packages)
;;               #:use-module (guix download)
;;               #:use-module (guix git-download)
;;               #:use-module (guix build-system trivial)
;;               #:use-module ((guix licenses) #:prefix license:)
;;               #:use-module (gnu packages linux)
;;               #:use-module (srfi srfi-1))
;;
;;(define-public linux-nonfree
;;               (package
;;                 (inherit linux-libre)
;;                 (name "linux-nonfree")
;;                 (version (package-version linux-libre))
;;                 (source
;;                   (origin
;;                     (method url-fetch)
;;                     (uri
;;                       (string-append
;;                         "https://www.kernel.org/pub/linux/kernel/v4.x/"
;;                         "linux-" version ".tar.xz"))
;;                     (sha256
;;                       (base32
;;                         "1lm2s9yhzyqra1f16jrjwd66m3jl43n5k7av2r9hns8hdr1smmw4"))))))
;;
;;(define (linux-firmware-version) "9d40a17beaf271e6ad47a5e714a296100eef4692")
;;(define (linux-firmware-source version)
;;  (origin
;;    (method git-fetch)
;;    (uri (git-reference
;;           (url (string-append "https://git.kernel.org/pub/scm/linux/kernel"
;;                               "/git/firmware/linux-firmware.git"))
;;           (commit version)))
;;    (file-name (string-append "linux-firmware-" version "-checkout"))
;;    (sha256
;;      (base32
;;        "099kll2n1zvps5qawnbm6c75khgn81j8ns0widiw0lnwm8s9q6ch"))))
;;
;;(define-public linux-firmware-iwlwifi
;;               (package
;;                 (name "linux-firmware-iwlwifi")
;;                 (version (linux-firmware-version))
;;                 (source (linux-firmware-source version))
;;                 (build-system trivial-build-system)
;;                 (arguments
;;                   `(#:modules ((guix build utils))
;;                     #:builder (begin
;;                                 (use-modules (guix build utils))
;;                                 (let ((source (assoc-ref %build-inputs "source"))
;;                                       (fw-dir (string-append %output "/lib/firmware/")))
;;                                   (mkdir-p fw-dir)
;;                                   (for-each (lambda (file)
;;                                               (copy-file file
;;                                                          (string-append fw-dir (basename file))))
;;                                             (find-files source
;;                                                         "iwlwifi-.*\\.ucode$|LICENSE\\.iwlwifi_firmware$"))
;;                                   #t))))
;;                 (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
;;                 (synopsis "Non-free firmware for Intel wifi chips")
;;                 (description "Non-free iwlwifi firmware")
;;                 (license (license:non-copyleft
;;                            "https://git.kernel.org/cgit/linux/kernel/git/firmware/linux-firmware.git/tree/LICENCE.iwlwifi_firmware?id=HEAD"))))

(define this-file
  (local-file (basename (assoc-ref (current-source-location) 'filename))
	      "config.scm"))

;;(define *lspci*
;;  (let* ((port (open-pipe* OPEN_READ "lspci"))
;;         (str (get-string-all port)))
;;    (close-pipe port)
;;    str))

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
  ;;  (kernel (cond
  ;;            ((string-match "Network controller: Intel Corporation Wireless 7260"
  ;;                           *lspci*)
  ;;             linux-nonfree)
  ;;            (#t linux-libre)))
  ;;  (firmware (append (list linux-firmware-iwlwifi)
  ;;                    %base-firmware))
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
      (list (specification->package "i3-wm")
	    (specification->package "i3status")
	    (specification->package "dmenu")
	    (specification->package "st")
	    (specification->package "emacs")
	    (specification->package "emacs-exwm")
	    (specification->package
	      "emacs-desktop-environment")
	    (specification->package "nss-certs"))
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
		(target "/dev/nvme0n1p1")
		(keyboard-layout keyboard-layout)))
  (swap-devices (list "/dev/nvme0n1p2"))
  (mapped-devices
    (list (mapped-device
	    (source
	      (uuid "ff24d4d0-8a81-413a-b7e1-620519b049dc"))
	    (target "cryptroot")
	    (type luks-device-mapping))
	  (mapped-device
	    (source
	      (uuid "e8fa3c58-e87e-4f60-9ae4-91eb3cf8ee73"))
	    (target "crypthome")
	    (type luks-device-mapping))))

  (file-systems (cons* (file-system
			 (mount-point "/")
			 (device "/dev/mapper/cryptroot")
			 (type "ext4")
			 (dependencies mapped-devices))
		       (file-system
			 (mount-point "/home")
			 (device "/dev/mapper/crypthome")
			 (type "ext4")
			 (dependencies mapped-devices))
		       %base-file-systems)))
