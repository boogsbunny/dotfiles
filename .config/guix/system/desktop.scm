(define-module (desktop)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu services)
  #:use-module (gnu services sddm)
  #:use-module (gnu system keyboard)
  #:use-module (ice-9 match))

(define %unwanted-gnome-packages
  '(;; core shell
    "gnome-backgrounds"
    "gnome-themes-extra"
    "gnome-getting-started-docs"
    "gnome-user-docs"
    ;; core utilities
    "baobab"
    "cheese"
    "eog"
    "epiphany"
    "evince"
    "file-roller"
    "gedit"
    "gnome-boxes"
    "gnome-calculator"
    "gnome-calendar"
    "gnome-characters"
    "gnome-clocks"
    "gnome-contacts"
    "gnome-disk-utility"
    "gnome-font-viewer"
    "gnome-maps"
    "gnome-screenshot"
    "gnome-terminal"
    "gnome-weather"
    "simple-scan"
    "totem"
    ;; misc
    "gnome-online-accounts"))

(define-public %boogs/gnome-minimal
  (package
   (inherit gnome-shell)
   (name "gnome-minimal")
   (propagated-inputs
    (fold alist-delete
          (package-propagated-inputs gnome-shell)
          %unwanted-gnome-packages))))
