(define-module (home services calibre-web)
  #:use-module (gnu home services)
  #:use-module (gnu home services containers)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu packages base)
  #:use-module (guix gexp)
  #:export (home-calibre-web-configuration
            home-calibre-web-service-type))

(define-configuration/no-serialization home-calibre-web-configuration
  (image
   (string "lscr.io/linuxserver/calibre-web:latest")
   "OCI image reference for Calibre-Web.")

  (host-port
   (string "8083")
   "Host port to expose Calibre-Web on (container port is 8083).")

  (config-volume
   (string "calibre-web-config")
   "OCI volume name to persist /config.")

  (media-library
   (string "/media/personal/books/calibre")
   "Preferred library directory (used if it exists). Never created by this
service.")

  (home-library
   (string "/home/boogs/media/personal/books/calibre")
   "Fallback library directory. Created if missing.")

  (library-link
   (string "/home/boogs/.local/share/calibre-web/books")
   "Stable symlink path bind-mounted into the container as /books.")

  (environment
   (list-of-strings
    (list "TZ=America/New_York"
          "PUID=1000"
          "PGID=100"))
   "Environment variables passed to the container (linuxserver.io style)."))

(define (home-calibre-web-activation config)
  (let ((media (home-calibre-web-configuration-media-library config))
        (home (home-calibre-web-configuration-home-library config))
        (link (home-calibre-web-configuration-library-link config)))
    (define mkdir* (file-append coreutils "/bin/mkdir"))
    (define rm* (file-append coreutils "/bin/rm"))
    (define ln* (file-append coreutils "/bin/ln"))
    (define test* (file-append coreutils "/bin/test"))
    #~(begin
        (define (dirname* path)
          (let ((i (string-rindex path #\/)))
            (cond
             ((not i) ".")
             ((= i 0) "/")
             (else (substring path 0 i)))))

        (define (dir? p)
          (zero? (system* #$test* "-d" p)))

        (define (exists? p)
          (zero? (system* #$test* "-e" p)))

        (define (symlink? p)
          (zero? (system* #$test* "-L" p)))

        ;; Prefer /media if it exists; otherwise ensure home path exists.
        (define target
          (if (dir? #$media)
              #$media
              (begin
                (system* #$mkdir* "-p" #$home) ; only creates under /home
                #$home)))

        ;; Make sure parent dir for link exists.
        (system* #$mkdir* "-p" (dirname* #$link))

        ;; If link exists:
        ;; - Remove it if it's a symlink or file
        ;; - If it's a directory, do NOT delete it (warn and leave it)
        (when (exists? #$link)
          (cond
           ((symlink? #$link)
            (system* #$rm* "-f" #$link))
           ((dir? #$link)
            (begin
              (display "calibre-web: library-link exists as a directory; not replacing it:\n")
              (display #$link)
              (newline)))
           (else
            (system* #$rm* "-f" #$link))))

        ;; Create the symlink if we can.
        (when (not (dir? #$link))
          (system* #$ln* "-s" target #$link))

        #t)))

(define (home-calibre-web->oci-extension config)
  (let ((image (home-calibre-web-configuration-image config))
        (host-port (home-calibre-web-configuration-host-port config))
        (cfg-vol (home-calibre-web-configuration-config-volume config))
        (link (home-calibre-web-configuration-library-link config))
        (env (home-calibre-web-configuration-environment config)))
    (oci-extension
     (volumes (list (oci-volume-configuration (name cfg-vol))))
     (containers
      (list
       (oci-container-configuration
        (image image)
        (provision "calibre-web")
        (ports (list (cons host-port "8083")))
        (environment env)
        (volumes
         (list (string-append cfg-vol ":/config")
               (string-append link ":/books")))))))))

(define home-calibre-web-service-type
  (service-type
   (name 'home-calibre-web)
   (description "Run Calibre-Web as an OCI container under the user's Shepherd.")
   (extensions
    (list (service-extension home-activation-service-type
                             home-calibre-web-activation)
          (service-extension home-oci-service-type
                             home-calibre-web->oci-extension)))
   (default-value (home-calibre-web-configuration))))
