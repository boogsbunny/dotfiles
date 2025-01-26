(use-modules (srfi srfi-1)
             (ice-9 textual-ports)
             (ice-9 popen))

(define (find-guix-channel channel-list)
  (find
   (lambda (channel) (eq? (channel-name channel) 'guix))
   channel-list))

(define current-guix (find-guix-channel %default-channels))

(define (current-guix-commit)
  (let* ((port (open-input-pipe "guix describe -f channels"))
         (str (get-string-all port)))
    (close-pipe port)
    (channel-commit
     (find-guix-channel (eval-string str)))))

(define* (channel-list #:key guix-commit)
  (cons*
   (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    (introduction
     (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
      (openpgp-fingerprint
       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))
    (branch "master"))
   (channel
    (name 'boogs)
    (url "https://github.com/boogsbunny/boogs")
    (introduction
     (make-channel-introduction
      "bfba2649c078eed68df6c397bc7b2314a3885780"
      (openpgp-fingerprint
       "FE32 E2EC 5E93 703E 5E1E  BFA5 437E C8EA 278E 5FEF")))
    (branch "master"))
   (cons
    (if guix-commit
        (channel
         (inherit current-guix)
         (commit guix-commit))
        current-guix)
    (delete current-guix %default-channels))))

(channel-list)
