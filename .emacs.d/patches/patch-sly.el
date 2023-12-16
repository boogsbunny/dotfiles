(with-eval-after-load 'sly-asdf
  (defun sly-asdf-load-system (&optional system)
    "Compile and load an ASDF SYSTEM.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
    (interactive (list (sly-asdf-read-system-name)))
    (sly-asdf-oos system 'load-op :force nil)))

(provide 'patch-sly)
