(use-modules
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-26))

(define* (mapconcat proc lst #:optional (separator ""))
  "Apply PROC to each element of LST and concatenate the result strings
into a single string using SEPARATOR."
  (match lst
    (() "")
    ((elt . rest)
     (fold (lambda (elt res)
             (string-append res separator (proc elt)))
           (proc elt)
           rest))))

(define (build-file-name . file-parts)
  "Return file name by concatenating FILE-PARTS with slashes."
  (mapconcat identity file-parts "/"))

(define (home-file . file-parts)
  "Return file name from my home directory."
  (apply build-file-name (getenv "HOME") file-parts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define xrdb
  (make <service>
    #:docstring "X resources for xterm.
Some DM merge this automatically; we merge it for the others."
    #:provides '(xrdb)
    #:start (make-system-constructor
             (string-join (list "xrdb" "-merge" (home-file ".Xresources"))))
    #:one-shot? #t))

(define no-bell
  (make <service>
    #:provides '(no-bell)
    #:start (make-system-constructor "xset -b")
    #:one-shot? #t))

(define auto-lock
  (make <service>
    #:provides '(auto-lock)
    #:start (make-system-constructor "xss-lock slock &")
    #:stop (make-system-destructor "pkill xss-lock")
    #:respawn? #t))

;; (define auto-mount
;;   (make <service>
;;     #:provides '(auto-mount)
;;     #:start (make-system-constructor "udiskie &")
;;     #:stop (make-system-destructor "pkill udiskie")
;;     #:respawn? #t))

;; (define cron
;;   (make <service>
;;     #:provides '(cron)
;;     #:docstring "Crontab manager.
;; Start after PATH is fully set or else local programs could
;; be missing."
;;     #:start (make-system-constructor "mcron &")
;;     #:stop (make-system-destructor "pkill mcron")
;;    #:respawn? #t))

;; (define location-munich "48.13:11.58")
(define location-nova "38.87:-77.42")

(define redshift
  (make <service>
    #:provides '(redshift)
    #:docstring "Redshift adjusts the color temperature of your screen according
to your surroundings.  This may help your eyes hurt less if you are working in
front of the screen at night."
    #:start (make-forkexec-constructor
             (list "redshift" "-l" location-nova "-t 3500:1500")
             #:log-file (string-append
                         (or (getenv "XDG_CONFIG_HOME")
                             (string-append (getenv "HOME") "/.config"))
                         "/redshift/redshift.log"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

;; (define main-services (list
;;                        auto-mount
;;                        cron))

(define display-services (list
                          xrdb
                          no-bell
                          auto-lock
                          redshift))
