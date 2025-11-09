(define-module (backup)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (guix gexp)
  #:export (%rsync-backup-job
            %btrfs-snapshot-job
            %garbage-collector-job
            %backup-ignore
            %backup-paths
            %backup-services))

(define-public %backup-paths
  '((".gnupg" . "-avx")
    (".password-store" . "-avx")
    ("Pictures" . "-avx")
    ("common-lisp" . "-avx")
    ("dotfiles" . "-avx")
    ("grapheneboogs" . "-avx")
    ("projects" . "-avLx --exclude 'node_modules'")
    ("repos" . "-avLx --exclude 'node_modules'")))

(define-public %backup-ignore
  '(("common-lisp" . ("source"))
    ("dotfiles" . ("*.iso"))))

(define %rsync-backup-job
  #~(begin
      (use-modules (ice-9 format))

      (define (log-backup msg)
        (format #t "backup: ~a\n" msg))

      (define home-directory "/home/boogs")

      (define media-directory
        (if (file-exists? "/media")
            "/media/home/"
            (string-append home-directory "/media/home/")))

      (define (make-rsync-command path opts)
        (let* ((source-path (string-append home-directory "/" path))
               (target-path (string-append media-directory path))
               (target-dir (dirname target-path))
               (rsync-cmd (string-append "rsync " opts)))
          (log-backup
           (format #f "Checking home path: ~a" source-path))

          (define (extra-exclude-args)
            (let ((subdirs (assoc-ref '#$%backup-ignore path)))
              (if subdirs
                  (string-join
                   (map (lambda (d)
                          (format #f "--exclude '~a'" d))
                        subdirs)
                   " ")
                  "")))

          (define (exit-code status) (quotient status 256))

          (if (file-exists? source-path)
              (begin
                (unless (file-exists? target-dir)
                  (mkdir-p target-dir))
                (log-backup
                 (format #f "Backing up ~a to ~a"
                         source-path
                         media-directory))
                (let* ((cmd (format #f "~a ~a --progress ~a ~a"
                                    rsync-cmd
                                    (extra-exclude-args)
                                    source-path
                                    media-directory)))
                  (log-backup (format #f "Executing: ~a" cmd))
                  (let ((status (system cmd)))
                    (cond ((zero? status)
                           (log-backup "Success"))
                          ((= (exit-code status) 23)
                           (log-backup "Warning: rsync returned 23 (partial transfer)."))
                          (else
                           (error
                            (format #f "Rsync failed with exit code ~a (status ~a)"
                                    (exit-code status) status)))))))
              (begin
                (log-backup
                 (format #f "Skipping non-existent path: ~a"
                         source-path))
                #f))))

      (job '(next-hour '(19))
           (lambda ()
             (log-backup "Starting backup...")
             (for-each
              (lambda (path-opts)
                (make-rsync-command (car path-opts) (cdr path-opts)))
              '#$%backup-paths)
             (log-backup "Backup completed"))
           "backup")))

(define %btrfs-snapshot-job
  #~(begin
      (define btrfs/home-directory "/home/boogs")

      (define btrfs/media-directory
        (if (file-exists? "/media")
            "/media/"
            (string-append btrfs/home-directory "/media/")))

      (define (make-snapshot-command subvol timestamp)
        (format #f
                "btrfs subvolume snapshot -r ~a~a ~
                 ~a.snapshots/~a/~a"
                btrfs/media-directory
                subvol
                btrfs/media-directory
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
  #~(begin
      (use-modules (ice-9 format))

      (define (log-gc msg)
        (format #t "gc: ~a\n" msg))

      (job '(next-hour '(21))
           (lambda ()
             (log-gc "Starting garbage collection")
             (system "guix gc -F 30G")
             (log-gc "Garbage collection completed"))
           "gc")))

(define-public %backup-services
  (list (simple-service 'boogs/cron-jobs mcron-service-type
                        (list
                         %rsync-backup-job
                         %btrfs-snapshot-job
                         %garbage-collector-job))))
