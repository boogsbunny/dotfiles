(define-module (backup)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (guix gexp))

(define-public %backup-paths
  '(("repos" . "-avLx --exclude 'node_modules'")
    ("projects" . "-avLx --exclude 'node_modules'")
    ("dotfiles" . "-avx")
    ("common-lisp" . "-avx")
    (".gnupg" . "-avx")
    (".password-store" . "-avx")
    ("grapheneboogs" . "-avx")
    ("Pictures" . "-avx")))

(define %rsync-backup-job
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

(define %btrfs-snapshot-job
  #~(begin
      (define home-directory
        (or (getenv "HOME")
            (error "HOME environment variable not set")))

      (define media-directory
        (if (file-exists? "/media")
            "/media/"
            (string-append home-directory "/media/")))

      (define (make-snapshot-command subvol timestamp)
        (format #f
                "btrfs subvolume snapshot -r ~a~a ~
                 ~a.snapshots/~a/~a"
                media-directory
                subvol
                media-directory
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

(define %garbage-collector-job
  ;; Collect garbage 5 minutes after midnight every day.
  ;; The job's action is a shell command.
  #~(job "5 0 * * *" ; Vixie cron syntax
         "guix gc -F 1G"))

(define-public %backup-services
  (list (simple-service 'boogs/cron-jobs mcron-service-type
                        (list
                         %rsync-backup-job
                         %btrfs-snapshot-job
                         %garbage-collector-job))))
