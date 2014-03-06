;;;; Feed archiver.

(in-package :athens)

(defvar *configuration* nil
  "Current configuration.")

(defvar *debug* nil
  "Debug mode.")

(defmacro with-configuration (configuration &body body)
  "Evaluate BODY using CONFIGURATION."
  `(let* ((*configuration* ,configuration)
          (*debug* (getf *configuration* :debug)))
     (with-database (getf *configuration* :database)
       ,@body)))

(defmacro with-configuration-file (path &body body)
  "Evaluate BODY using configuration from PATH."
  `(with-configuration (import-configuration ,path)
     ,@body))

(defun initialize-database ()
  "Initialize database (needs to be called once to setup an empty
datatabase)."
  (create-feed-table)
  (create-item-table)
  (create-log-table)
  (create-global-date-table)
  (values))

(defun add-feed (url)
  "Add feed at URL to archive."
  (insert-feed (url-hash url) `(:source ,url :date 0)))

(defun remove-feed (feed-hash)
  "Remove feed by FEED-HASH."
  (delete-feed feed-hash))

(defmacro with-flexi-use-value (&body body)
  "Bind FLEXI-STREAM's USE-VALUE restart during BODY."
  `(handler-bind
       ((external-format-encoding-error
         #'(lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'use-value #\Replacement_Character))))
     ,@body))

(defun headers (date)
  "Construct headers for «If-Modified-Since» request for DATE and
includes «Accept-Charset» to favor UTF-8."
  `(("If-Modified-Since" . ,(universal-time-to-http-date date))
    ("Accept-Charset" . "utf-8, *:q=0.5" )))

(defun get-if-modified-since (url date)
  "Return HTTP response body of URL or NIL if it hasn't been
modified since DATE."
  (multiple-value-bind (body status)
      (with-flexi-use-value
        (http-request url
                      :additional-headers (headers date)
                      :connection-timeout 10))
    (case status
      (200 body)
      (304 nil)
      (otherwise
       (error "Could not retrieve ~a: Status ~a." url status)))))

(defun decode (data)
  "Try to decode DATA to a string if necessary."
  (etypecase data
    (string data)
    ((array (unsigned-byte 8))
     (with-flexi-use-value
       (octets-to-string data :external-format :utf-8)))))

(defmacro with-skip-errors (identifier &body body)
  "Skip and log errors for IDENTIFIER during BODY."
  `(if (not *debug*)
       (handler-case (progn ,@body)
         (error (condition)
           (format *error-output* "~&Skipping ~a because: ~S ~a~%"
                   ,identifier condition condition)))
       (progn ,@body)))

(defun record-new-items (items feed)
  "Record ITEMS for FEED unless they are already recorded and return
list of logged item hashes."
  (loop for item in items
     for item-hash = (url-hash (getf item :link))
     for recorded-p = (unless (item-recorded-p item-hash)
                        (with-skip-errors item-hash
                          (record-item item-hash (list* :feed feed item))
                          t))
     when recorded-p
     collect item-hash))

(defun import-feed (url &optional (date 0))
  "Import feed at URL and return import log unless it has not been
modified since DATE, in that case return NIL."
  (let ((body (get-if-modified-since url date)))
    (if body
        (destructuring-bind (&key items link date title description)
            (parse-feed (decode body))
          (let* ((feed-hash (url-hash url))
                 (item-hashes (record-new-items items feed-hash)))
            (update-feed feed-hash
                         (list :source url
                               :link link
                               :date date
                               :title title
                               :description description))
            (when item-hashes
              (list feed-hash item-hashes))))
        (format *error-output* "~&Skipping ~a: Not modified.~%" url))))

(defun update-archive ()
  "Log new items for every feed archived."
  (let ((imports
         (loop for feed-hash in (feed-index)
               for feed = (get-feed feed-hash)
               for url = (getf feed :source)
               for import-log = (with-skip-errors url
                                  (import-feed url (getf feed :date)))
            when import-log
            collect import-log)))
    (when imports
      (log-imports imports)
      (update-global-date))))

(defun update-archive-periodically
    (&optional (update-interval (or (getf *configuration*
                                          :update-interval)
                                    3600))) ; One hour.
  "Update archive periodically as defined UPDATE-INTERVAL which defaults
to :UPDATE-INTERVAL in *CONFIGURATION* or 3600 seconds."
  (loop do
       (unwind-protect nil
         ;; Protect from being interrupted during update.
         (update-archive))
       (sleep update-interval)))

(defun archive-log (&optional (start 0) end)
  "Return combined imports log ranging from START to END dates."
  (let ((table (make-hash-table :test #'equal))
        (imports (get-imports start end)))
    (loop for import in imports do
         (loop for (feed items) in import do
              (if (gethash feed table)
                  (setf (gethash feed table)
                        (append (gethash feed table items)))
                  (setf (gethash feed table) items))))
    (loop for feed being the hash-keys of table
         collect (list feed (gethash feed table)))))

(defun make-server ()
  "Return HTTPD0 server with ATHENS.RESOURCE-RESPONDER."
  (apply #'make-httpd (make-athens-responder *configuration*)
         (getf *configuration* :httpd0)))
