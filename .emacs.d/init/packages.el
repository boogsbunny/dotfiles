;;--------------------------------;
;; Packages
;;--------------------------------;

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; update packages
;;(unless package-archive-contents
;;  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  ;; only fetch the archives if you don't have use-package installed
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
