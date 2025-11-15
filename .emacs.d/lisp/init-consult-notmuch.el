;;; -*- lexical-binding: t; -*-
;;--------------------------------------------------------------------
;; consult-notmuch
;;--------------------------------------------------------------------

(require 'consult)
(require 'notmuch)
(eval-when-compile (require 'cl-lib))
(require 'subr-x)

(declare-function vertico--candidate "vertico")
(declare-function vertico-next "vertico")
(declare-function vertico-previous "vertico")

(defgroup consult-notmuch nil
  "Options for `consult-notmuch'."
  :group 'consult)

(defcustom consult-notmuch-show-single-message t
  "Show only the matching message or the whole thread in listings."
  :type 'boolean)

(defcustom consult-notmuch-result-format
  '(("date" . "%12s  ")
    ("count" . "%-7s ")
    ("authors" . "%-20s")
    ("subject" . "  %-54s")
    ("tags" . " (%s)"))
  "Format for matching candidates in minibuffer.
Supported fields are: date, authors, subject, count and tags."
  :type '(alist :key-type string :value-type string))

(defcustom consult-notmuch-newest-first t
  "List messages newest first (defaults to oldest first)."
  :type 'boolean)

(defcustom consult-notmuch-min-input consult-async-min-input
  "Minimum input length to launch a notmuch search."
  :type '(natnum :tag "Number of characters"))

(defcustom consult-notmuch-default-inbox-query
  "tag:inbox AND NOT tag:trash AND NOT tag:spam AND date:1w.."
  "Default query used by `consult-notmuch-inbox'"
  :type 'string)

(defvar consult-notmuch-tag-history nil
  "History for tag prompts in consult-notmuch")

(defvar consult-notmuch-history nil
  "History for `consult-notmuch'.")

(defvar consult-notmuch--partial-parse nil
  "Internal variable for parsing status.")

(defvar consult-notmuch--partial-headers nil
  "Internal variable for parsing status.")

(defvar consult-notmuch--info nil
  "Internal variable for parsing status.")

(defvar consult-notmuch--buffer-name "*consult-notmuch*"
  "Name of preview and result buffers.")

(defvar consult-notmuch--thread-cache (make-hash-table :test #'equal)
  "Cache mapping notmuch message selectors (id:...) to thread
ids (thread:...).")

(defun consult-notmuch--command (input)
  "Construct a search command for emails containing INPUT."
  (let ((sort (if consult-notmuch-newest-first
                  "--sort=newest-first"
                "--sort=oldest-first")))
    (if consult-notmuch-show-single-message
        `(,notmuch-command "show" "--body=false" ,sort ,input)
      `(,notmuch-command "search" ,sort ,input))))

(defun consult-notmuch--thread-of-candidate (cand)
  "Return thread selector (thread:...) for CAND.
For thread-mode candidates, the 'id property is already a thread.
For message-mode candidates, resolve via notmuch and cache."
  (when cand
    (let ((id (get-text-property 0 'id cand)))
      (cond
       ((and id (string-prefix-p "thread:" id)) id)
       ((and id (string-prefix-p "id:" id))
        (or (gethash id consult-notmuch--thread-cache)
            (let* ((out (with-output-to-string
                          (call-process
                           notmuch-command nil standard-output nil
                           "search" "--output=threads" id)))
                   (thread (car (split-string (string-trim out)))))
              (when (and thread (string-prefix-p "thread:" thread))
                (puthash id thread consult-notmuch--thread-cache))
              thread)))
       (t nil)))))

(defun consult-notmuch--move (fn)
  "Call FN (e.g., `vertico-next' or `vertico-previous').
Return non-nil if the candidate actually changed."
  (let ((before (ignore-errors (vertico--candidate))))
    (funcall fn)
    (let ((after (ignore-errors (vertico--candidate))))
      (not (equal before after)))))

(defun consult-notmuch--goto-next-thread-first ()
  (unless (and (fboundp 'vertico--candidate)
               (fboundp 'vertico-next))
    (user-error "Thread navigation requires Vertico"))
  (let* ((limit 4000)
         (i 0)
         (cur (ignore-errors (vertico--candidate)))
         (cur-thread (consult-notmuch--thread-of-candidate cur)))
    (when (consult-notmuch--move #'vertico-next)
      (cl-loop while (< i limit)
               do (setq i (1+ i))
               for cand = (ignore-errors (vertico--candidate))
               for tid = (and cand
                              (consult-notmuch--thread-of-candidate
                               cand))
               if (and cur-thread tid (string= tid cur-thread))
               do (unless (consult-notmuch--move #'vertico-next)
                    (cl-return))
               else do (cl-return)))))

(defun consult-notmuch--goto-previous-thread-first ()
  (unless (and (fboundp 'vertico--candidate)
               (fboundp 'vertico-previous))
    (user-error "Thread navigation requires Vertico"))
  (let* ((limit 8000)
         (i 0)
         (cur (ignore-errors (vertico--candidate)))
         (cur-thread (consult-notmuch--thread-of-candidate cur)))
    (when (consult-notmuch--move #'vertico-previous)
      (let ((cand (ignore-errors (vertico--candidate)))
            (tid nil))
        (setq tid (and cand
                       (consult-notmuch--thread-of-candidate cand)))
        (cl-loop while (< i limit)
                 do (setq i (1+ i))
                 if (and cur-thread tid (string= tid cur-thread))
                 do (unless (consult-notmuch--move #'vertico-previous)
                      (cl-return))
                 else do (cl-return)))
      (let ((target (consult-notmuch--thread-of-candidate
                     (ignore-errors (vertico--candidate))))
            (stuck nil))
        (when target
          (setq i 0)
          (while (and (< i limit) (not stuck))
            (setq i (1+ i))
            (let ((before (ignore-errors (vertico--candidate))))
              (if (consult-notmuch--move #'vertico-previous)
                  (let* ((cand (ignore-errors (vertico--candidate)))
                         (tid (and cand
                                   (consult-notmuch--thread-of-candidate
                                    cand))))
                    (unless (and tid (string= tid target))
                      (consult-notmuch--move #'vertico-next)
                      (setq stuck t)))
                (setq stuck t)))))))))

(defun consult-notmuch-next-thread ()
  (interactive)
  (consult-notmuch--goto-next-thread-first))

(defun consult-notmuch-previous-thread ()
  (interactive)
  (consult-notmuch--goto-previous-thread-first))

(defvar consult-notmuch--minibuffer-map
  (let* ((base (when (boundp 'vertico-map) vertico-map))
         (map (make-sparse-keymap))
         (composed (if base (make-composed-keymap map base) map)))
    (define-key composed (kbd "C-j") #'consult-notmuch-next-thread)
    (define-key composed (kbd "C-k") #'consult-notmuch-previous-thread)
    composed))

(defvar consult-notmuch--tag-cache nil
  "Cached list of tags returned by notmuch.")

(defun consult-notmuch-tags-refresh ()
  "Refresh the cached list of tags."
  (interactive)
  (setq consult-notmuch--tag-cache nil)
  (message "consult-notmuch: tag cache cleared"))

(defun consult-notmuch--all-tags ()
  "Return all tags from notmuch, possibly using a cache."
  (or consult-notmuch--tag-cache
      (setq consult-notmuch--tag-cache
            (let* ((output (with-output-to-string
                             (call-process
                              notmuch-command nil standard-output nil
                              "search" "--output=tags" "--exclude=false" "*")))
                   (lines (split-string output "\n" t)))
              (cl-remove-duplicates lines :test #'string=)))))

(defun consult-notmuch--read-tags (prompt)
  "Read one or more tags with completion."
  (let ((crm-separator "[ ,]+"))
    (completing-read-multiple
     prompt
     (consult-notmuch--all-tags)
     nil t nil 'consult-notmuch-tag-history)))

(defun consult-notmuch--search (&optional initial)
  "Perform an asynchronous notmuch search via `consult--read'.
If given, use INITIAL as the starting point of the query."
  (setq consult-notmuch--partial-parse nil)
  (consult--read (consult--async-pipeline
                  (consult--process-collection #'consult-notmuch--command
                                               :min-input
                                               consult-notmuch-min-input)
                  (consult--async-map #'consult-notmuch--transformer)
                  (consult--async-filter #'identity))
                 :prompt "Notmuch search: "
                 :require-match t
                 :initial initial
                 :keymap consult-notmuch--minibuffer-map
                 :history '(:input consult-notmuch-history)
                 :state #'consult-notmuch--preview
                 :lookup #'consult--lookup-member
                 :category 'notmuch-result
                 :sort nil))

(defun consult-notmuch--transformer (str)
  "Transform STR to notmuch display style."
  (if consult-notmuch-show-single-message
      (consult-notmuch--show-transformer str)
    (consult-notmuch--search-transformer str)))

(defun consult-notmuch--format-field (spec msg)
  "Return a string for SPEC given the MSG metadata."
  (let ((field (car spec)))
    (cond ((equal field "count")
           (when-let (cnt (plist-get msg :count))
             (format (cdr spec) cnt)))
          ((equal field "tags")
           (when (plist-get msg :tags)
             (notmuch-tree-format-field "tags" (cdr spec) msg)))
          (t (notmuch-tree-format-field field (cdr spec) msg)))))

(defun consult-notmuch--format-candidate (msg)
  "Format the result (MSG) of parsing a notmuch show information unit."
  (when-let (id (plist-get msg :id))
    (let ((result-string))
      (dolist (spec consult-notmuch-result-format)
        (when-let (field (consult-notmuch--format-field spec msg))
          (setq result-string (concat result-string field))))
      (propertize
       result-string
       'id id
       'tags (plist-get msg :tags)
       'subject (plist-get (plist-get msg :headers) :Subject)))))

(defun consult-notmuch--candidate-id (candidate)
  "Recover the thread id for the given CANDIDATE string."
  (when candidate (get-text-property 0 'id candidate)))

(defun consult-notmuch--candidate-tags (candidate)
  "Recover the message tags for the given CANDIDATE string."
  (when candidate (get-text-property 0 'tags candidate)))

(defun consult-notmuch--set (k v)
  "Set the value V for property K in the message we're currently parsing."
  (setq consult-notmuch--partial-parse
        (plist-put consult-notmuch--partial-parse k v)))

(defun consult-notmuch--show-transformer (str)
  "Parse output STR of notmuch show, extracting its components."
  (if (string-prefix-p "message}" str)
      (prog1
          (consult-notmuch--format-candidate
           (consult-notmuch--set :headers consult-notmuch--partial-headers))
        (setq consult-notmuch--partial-parse nil
              consult-notmuch--partial-headers nil
              consult-notmuch--info nil))
    (cond ((string-match "message{ \\(id:[^ ]+\\) .+" str)
           (consult-notmuch--set :id (match-string 1 str))
           (consult-notmuch--set :match t))
          ((string-prefix-p "header{" str)
           (setq consult-notmuch--info t))
          ((and str consult-notmuch--info)
           (when (string-match "\\(.+\\) (\\([^)]+\\)) (\\([^)]*\\))$" str)
             (consult-notmuch--set :Subject (match-string 1 str))
             (consult-notmuch--set :date_relative (match-string 2 str))
             (consult-notmuch--set :tags (split-string (match-string 3 str))))
           (setq consult-notmuch--info nil))
          ((string-match "\\(Subject\\|From\\|To\\|Cc\\|Date\\): \\(.+\\)?" str)
           (let ((k (intern (format ":%s" (match-string 1 str))))
                 (v (or (match-string 2 str) "")))
             (setq consult-notmuch--partial-headers
                   (plist-put consult-notmuch--partial-headers k v)))))
    nil))

(defun consult-notmuch--search-transformer (str)
  "Transform STR from notmuch search to notmuch display style."
  (when (string-match "thread:" str)
    (let* ((id (car (split-string str "\\ +")))
           (date (substring str 24 37))
           (mid (substring str 24))
           (c0 (string-match "[[]" mid))
           (c1 (string-match "[]]" mid))
           (count (substring mid c0 (1+ c1)))
           (auths (string-trim (nth 1 (split-string mid "[];]"))))
           (subject (string-trim (nth 1 (split-string mid "[;]"))))
           (headers (list :Subject subject :From auths))
           (t0 (string-match "([^)]*)\\s-*$" mid))
           (tags (split-string (substring mid (1+  t0) -1)))
           (msg (list :id id
                      :match t
                      :headers headers
                      :count count
                      :date_relative date
                      :tags tags)))
      (consult-notmuch--format-candidate msg))))

(defun consult-notmuch--show-id (id buffer)
  "Show message or thread id in the requested buffer"
  (let ((notmuch-show-only-matching-messages
         consult-notmuch-show-single-message))
    (notmuch-show id nil nil nil buffer)))

(defun consult-notmuch--preview (action candidate)
  "Preview CANDIDATE when ACTION is 'preview."
  (cond ((eq action 'preview)
         (when-let ((id (consult-notmuch--candidate-id candidate)))
           (when (get-buffer consult-notmuch--buffer-name)
             (kill-buffer consult-notmuch--buffer-name))
           (consult-notmuch--show-id id consult-notmuch--buffer-name)))
        ((eq action 'exit)
         (when (get-buffer consult-notmuch--buffer-name)
           (kill-buffer consult-notmuch--buffer-name)))))

(defun consult-notmuch--show (candidate)
  "Open resulting CANDIDATE in ‘notmuch-show’ view."
  (when-let ((id (consult-notmuch--candidate-id candidate)))
    (let* ((subject (or (get-text-property 0 'subject candidate) ""))
           (title (string-trim (concat consult-notmuch--buffer-name " " subject))))
      (consult-notmuch--show-id id title))))

(defun consult-notmuch--tree (candidate)
  "Open resulting CANDIDATE in ‘notmuch-tree’."
  (when-let ((thread-id (consult-notmuch--candidate-id candidate)))
    (notmuch-tree thread-id nil nil)))

;;;###autoload
(defun consult-notmuch-by-tags-all ()
  "Prompt for tags and search for messages that have ALL of them (AND)."
  (interactive)
  (let ((tags (consult-notmuch--read-tags "Notmuch tags (ALL, AND): ")))
    (when tags
      (consult-notmuch
       (mapconcat (lambda (t) (format "tag:%s" t)) tags " AND ")))))

;;;###autoload
(defun consult-notmuch-by-tags-any ()
  "Prompt for tags and search for messages that have ANY of them (OR)."
  (interactive)
  (let ((tags (consult-notmuch--read-tags "Notmuch tags (ANY, OR): ")))
    (when tags
      (consult-notmuch
       (mapconcat (lambda (t) (format "tag:%s" t)) tags " OR ")))))

;;;###autoload
(defun consult-notmuch-by-tags-without ()
  "Prompt for tags and search for messages WITHOUT those tags (NOT)."
  (interactive)
  (let ((tags (consult-notmuch--read-tags "Notmuch tags (WITHOUT, NOT): ")))
    (when tags
      (consult-notmuch
       (mapconcat (lambda (t) (format "NOT tag:%s" t)) tags " AND ")))))

;;;###autoload
(defun consult-notmuch (&optional initial)
  "Search for your email in notmuch, showing single messages.
If given, use INITIAL as the starting point of the query."
  (interactive)
  (consult-notmuch--show (consult-notmuch--search initial)))

;;;###autoload
(defun consult-notmuch-tree (&optional initial)
  "Search for your email in notmuch, showing full candidate tree.
If given, use INITIAL as the starting point of the query."
  (interactive)
  (consult-notmuch--tree (consult-notmuch--search initial)))

;;;###autoload
(defun consult-notmuch-inbox ()
  "Open consult-notmuch with your default inbox query."
  (interactive)
  (consult-notmuch consult-notmuch-default-inbox-query))

(with-eval-after-load 'embark
  (defvar consult-notmuch-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "+") 'consult-notmuch-tag)
      (define-key map (kbd "-") 'consult-notmuch-tag)
      map)
    "Keymap for actions on Notmuch entries.")

  (set-keymap-parent consult-notmuch-map embark-general-map)
  (add-to-list 'embark-keymap-alist '(notmuch-result . consult-notmuch-map))

  (defun consult-notmuch--address-to-multi-select (address)
    "Select more email addresses, in addition to the current selection"
    (consult-notmuch-address t address))

  (defvar consult-notmuch-address-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "c") #'consult-notmuch-address-compose)
      (define-key map (kbd "m") #'consult-notmuch--address-to-multi-select)
      map))

  (set-keymap-parent consult-notmuch-address-map embark-general-map)
  (add-to-list 'embark-keymap-alist
               '(notmuch-address . consult-notmuch-address-map))

  (defun consult-notmuch-tag (msg)
    (when-let* ((id (consult-notmuch--candidate-id msg))
                (tags (consult-notmuch--candidate-tags msg))
                (tag-changes (notmuch-read-tag-changes tags "Tags: " "+")))
      (notmuch-tag (concat "(" id ")") tag-changes)))

  (defvar consult-notmuch-export-function #'notmuch-search
    "Function used to ask notmuch to display a list of found ids.
  Typical options are notmuch-search and notmuch-tree.")

  (defun consult-notmuch-export (msgs)
    "Create a notmuch search buffer listing messages."
    (funcall consult-notmuch-export-function
     (concat "(" (mapconcat #'consult-notmuch--candidate-id msgs " ") ")")))
  (add-to-list 'embark-exporters-alist
               '(notmuch-result . consult-notmuch-export)))

(defun consult-notmuch--address-command (input)
  "Spec for an async command querying a notmuch address with INPUT."
  `(,notmuch-command "address" "--format=text" ,input))

(defun consult-notmuch-address-compose (address)
  "Compose an email to a given ADDRESS."
  (let ((other-headers (and notmuch-always-prompt-for-sender
                            `((From . ,(notmuch-mua-prompt-for-sender))))))
    (notmuch-mua-mail address
                      nil
                      other-headers
                      nil
                      (notmuch-mua-get-switch-function))))

(defun consult-notmuch--address-prompt ()
  (consult--read (consult--async-pipeline
                  (consult--process-collection
                   #'consult-notmuch--address-command))
                 :prompt "Notmuch addresses: "
                 :sort nil
                 :category 'notmuch-address))

;;;###autoload
(defun consult-notmuch-address (&optional multi-select-p initial-addr)
  "Search the notmuch db for an email address and compose mail to it.
With a prefix argument, prompt multiple times until there
is an empty input."
  (interactive "P")
  (if multi-select-p
      (cl-loop for addr = (consult-notmuch--address-prompt)
               until (eql (length addr) 0)
               collect addr into addrs
               finally (consult-notmuch-address-compose
                        (mapconcat #'identity
                                   (if initial-addr
                                       (cons initial-addr addrs)
                                     addrs)
                                   ", ")))
    (consult-notmuch-address-compose (consult-notmuch--address-prompt))))

(defun consult-notmuch--interesting-buffers ()
  "Return a list of names of buffers with interesting notmuch data."
  (consult--buffer-query
   :as #'buffer-name
   :predicate #'notmuch-interesting-buffer))

;;;###autoload
(defvar consult-notmuch-buffer-source
  '(:name "Notmuch Buffer"
    :narrow (?n . "Notmuch")
    :hidden t
    :category 'buffer
    :face consult-buffer
    :history buffer-name-history
    :state consult--buffer-state
    :items consult-notmuch--interesting-buffers)
  "Notmuch buffer candidate source for `consult-buffer'.")

(add-to-list 'consult-buffer-sources 'consult-notmuch-buffer-source 'append)

(provide 'init-consult-notmuch)
