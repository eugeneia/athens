;;;; ATHENS.IMPORT: Feed importer.

(in-package :athens)

(defvar *debug* nil
  "Debug mode.")

(defun headers (date)
  "Construct headers for «If-Modified-Since» request for DATE and
includes «Accept-Charset» to favor UTF-8."
  `(("If-Modified-Since" . ,(universal-time-to-http-date date))
    ("Accept-Charset" . "utf-8, *:q=0.5" )))

(defun get-if-modified-since (url date)
  "Return HTTP response body of URL or NIL if it hasn't been
modified since DATE."
  (multiple-value-bind (body status)
      (http-request url :additional-headers (headers date))
    (case status
      (200 body)
      (304 nil)
      (otherwise (error "Could not retrieve ~a: Status ~a." url status)))))

(defmacro with-skip-errors (identifier &body body)
  "Skip and log errors for IDENTIFIER during BODY."
  `(if (not *debug*)
       (handler-case (progn ,@body)
         (error (condition)
           (format *debug-io* "~&Skipping ~a because: ~S ~a~%"
                   ,identifier condition condition)))
       (progn ,@body)))

(defun record-new-items (items feed)
  "Record ITEMS for FEED unless they are already recorded and return
list of logged item hashes."
  (loop for item in items
     for item-hash = (feed-item-hash item)
     for recorded-p = (unless (item-recorded-p item-hash)
                        (with-skip-errors item-hash
                          (record-item item-hash (list* :feed feed item))
                          t))
     when recorded-p
     collect item-hash))

(defun decode (data)
  "Try to decode DATA to a string if necessary."
  (etypecase data
    (string data)
    ((array (unsigned-byte 8))
     (handler-bind
         ((external-format-encoding-error
           #'(lambda (condition)
               (invoke-restart 'use-value #\Replacement_Character))))
       (octets-to-string data :external-format :utf-8)))))

(defun import-feed (url &optional (date 0))
  "Import feed at URL and return import log unless it has not been
modified since DATE, in that case return NIL."
  (let ((body (get-if-modified-since url date)))
    (if body
        (destructuring-bind (&key items link date title description)
            (parse-feed (decode body))
          (let ((item-hashes (record-new-items items url)))
            (update-feed url (make-feed nil
                                        :link link
                                        :date date
                                        :title title
                                        :description description))
            (when item-hashes
              (list url item-hashes))))
        (format *debug-io* "~&Skipping ~a: Not modified.~%" url))))

(defmacro with-configuration (configuration &body body)
  "Execute BODY with CONFIGURATION."
  `(with-database (getf ,configuration :database)
     ,@body))

(defun initialize-database ()
  "Initialize database (needs to be called once to setup an empty
datatabase)."
  (create-feed-table)
  (create-item-table)
  (create-log-table)
  (values))

(defun add-feed (url)
  "Add feed at URL to archive."
  (insert-feed url '(:date 0)))

(defun update-archive ()
  "Log new items for every feed archived."
  (let ((imports
         (loop for url in (feed-index)
               for feed = (get-feed url)
               for import-log = (with-skip-errors url
                                  (import-feed url (getf feed :date)))
            when import-log
            collect import-log)))
    (when imports
      (log-imports imports))))
      
