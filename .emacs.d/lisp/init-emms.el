;;--------------------------------;
;; Music
;;--------------------------------;

(emms-minimalistic)
(emms-history-load)
(emms-mode-line-disable)

(setq emms-player-list (list emms-player-mpv)
      emms-source-file-default-directory "/media/personal/videos/lofi"
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
      ;; Cover thumbnails.
      emms-browser-covers 'emms-browser-cache-thumbnail-async)

(add-to-list 'emms-player-mpv-parameters "--no-audio-display")
(add-to-list 'emms-info-functions 'emms-info-cueinfo)

 (provide 'init-emms)
