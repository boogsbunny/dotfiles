;;--------------------------------;
;; Music
;;--------------------------------;


(emms-all)
(emms-history-load)

(setq emms-player-list (list emms-player-mpv)
      emms-source-file-default-directory (file-truename "/media/videos/")
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
      ;; Cover thumbnails.
      emms-browser-covers 'emms-browser-cache-thumbnail-async)
(add-to-list 'emms-player-mpv-parameters "--no-audio-display")
(add-to-list 'emms-info-functions 'emms-info-cueinfo)
