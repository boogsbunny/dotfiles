(in-package :nyxt-user)

(define-configuration status-buffer
  ((height 25)))

(define-configuration window
  ((message-buffer-height 0)))

;; Message buffer is the small line down below where all the messages
;; are displayed. echo-area in Emacs parlance?
(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "black"
         :color "white")))))))

;;; Color config for prompt-buffer (minibuffer in Emacs parlance).
(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((body
               :background-color "black"
               :color "white")
              ("#prompt-area"
               :background-color "black")
              ;; The area you input text in.
              ("#input"
               :background-color "white")
              (".source-name"
               :color "black"
               :background-color "#556B2F")
              (".source-content"
               :background-color "black")
              (".source-content th"
               :border "1px solid #556B2F"
               :background-color "black")
              ;; The currently highlighted option.
              ("#selection"
               :background-color "#CD5C5C"
               :color "black")
              (.marked :background-color "#8B3A3A"
                       :font-weight "bold"
                       :color "white")
              (.selected :background-color "black"
                         :color "white")))))))

;;; Internal (i.e. help, info, describe-* buffers). Usually work for
;;; simple HTML display, so I'm overriding lots of things there.
;;;
;;; Panel buffers are the same in regards to style.
;; (define-configuration (internal-buffer panel-buffer)
;;   ((style
;;     (str:concat
;;      %slot-default%
;;      (cl-css:css
;;       '((title
;;          :color "#CD5C5C")
;;         (body
;;          :background-color "black"
;;          :color "lightgray")
;;         (hr
;;          :color "darkgray")
;;         (a
;;          :color "#556B2F")
;;         (.button
;;          :color "lightgray"
;;          :background-color "#556B2F")))))))


;;; History-tree-mode is a mode used in `history-tree' and
;;; `buffer-history-tree' buffers. It's not enough to customize
;;; `internal-buffer' to cover it, thus I'm customizing it
;;; specifically.
(define-configuration nyxt/history-tree-mode:history-tree-mode
  ((nyxt/history-tree-mode::style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "black"
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "#556B2F")
        ;; Those three below are here to color the tree-branching list
        ;; markers in white.
        ("ul li::before"
         :background-color "white")
        ("ul li::after"
         :background-color "white")
        ("ul li:only-child::before"
         :background-color "white")))))))

(define-configuration nyxt/web-mode:web-mode
  ;; The style of highlighted boxes, e.g. link hints.
  ((nyxt/web-mode:highlighted-box-style
    (cl-css:css
     '((".nyxt-hint.nyxt-highlight-hint"
        :background "#CD5C5C"))))))

;;; Status buffer is the strip above the message buffer/echo area.
;;; Modeline in Emacs parlance.
(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            ;; Arrows on the left.
            '(("#controls"
               :border-top "1px solid white"
               :background-color "#CD5C5C")
              ;; To the right of the arrows.
              ("#url"
               :background-color "black"
               :color "white"
               :border-top "1px solid white")
              ;; Far to the right.
              ("#modes"
               :background-color "black"
               :border-top "1px solid white")
              ;; The center segment.
              ("#tabs"
               :background-color "#CD5C5C"
               :color "black"
               :border-top "1px solid white")))))))

;;; Dark is a simple mode for simple HTML pages to color those in a
;;; darker palette.
(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "black !important"
                :background-image "none !important"
                :color "white")
               (a
                :background-color "black !important"
                :background-image "none !important"
                :color "#556B2F !important"))))))


(define-configuration (buffer web-buffer nosave-buffer)
  ((default-modes (append '(;; dark-mode
                            vi-normal-mode)
                          %slot-default%))))

(define-configuration prompt-buffer
  ((default-modes (append '(vi-insert-mode) %slot-default%))))
