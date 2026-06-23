;; -*- lexical-binding: t; -*-
;;--------------------------------------------------------------------
;; contacts
;;--------------------------------------------------------------------

(defun boogs/build-contact-item (template-string contact-property)
  (if-let ((stuff (org-entry-get nil contact-property)))
      (concat (format template-string stuff) "\n")
    ""))

(defun boogs/build-phone-items (phone-string)
  (let ((phones (split-string phone-string "," t " ")))
    (mapconcat
     (lambda (phone)
       (format "TEL;CELL:%s\n" (string-trim phone)))
     phones
     "")))

(defun boogs/vcard ()
  "Create a .vcf file containing all contact information."
  (interactive)
  (write-region
   (string-join
    (org-map-entries
     (lambda ()
       (let ((id (org-entry-get nil "ID")))
         (string-join
          `("BEGIN:VCARD\nVERSION:2.1\n"
            ,(format "UID:%s\n" id)
            ,(boogs/build-contact-item "FN:%s" "ITEM")
            ,(if-let ((phones (org-entry-get nil "PHONE")))
                 (boogs/build-phone-items phones)
               "")
            ,(boogs/build-contact-item "EMAIL:%s" "EMAIL")
            ,(boogs/build-contact-item "ORG:%s" "GROUP")
            ,(boogs/build-contact-item "ADR;HOME:;;%s" "ADDRESS_HOME")
            ,(boogs/build-contact-item "ADR;WORK:;;%s" "ADDRESS_WORK")
            ,(boogs/build-contact-item "BDAY:%s" "BIRTHDAY")
            ,(format "REV:%s\n" (format-time-string "%Y-%m-%dT%T"))
            "END:VCARD")
          "")))
     "LEVEL=1")
    "\n")
   nil
   (read-file-name
    "Where to save the .vcf file?"
    "~/bunny/"
    "contacts.vcf")))

(provide 'init-org-contacts)
