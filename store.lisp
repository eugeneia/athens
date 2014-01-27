;;;; ATHENS.STORE: Persistent logging and records.

(in-package :athens.store)

(defun db-spec
    (&key host (port 5432) user password (name "athens") (use-ssl :yes))
  "Return connection to database specified by parameters."
  `(,name ,user ,password ,host
    :port ,port :use-ssl ,use-ssl :pooled-p t))

(defmacro with-database (db-configuration &body body)
  `(with-connection (apply #'db-spec ,db-configuration)
          ,@body))

(defun create-feed-table ()
  (execute (sql (:create-table feed
                  ((url :type string :primary-key t)
                   (datum :type string))))))

(defun feed-index ()
  (mapcar #'car (query (sql (:select 'url :from 'feed)))))

(defprepared insert-feed%
    (:insert-into 'feed :set 'url '$1 'datum '$2))

(defun insert-feed (url feed)
  (insert-feed% url (prin1-to-string feed))
  (values))

(defprepared update-feed%
    (:update 'feed :set 'datum '$2 :where (:= 'url '$1)))

(defun update-feed (url feed)
  (update-feed% url (prin1-to-string feed))
  (values))

(defprepared get-feed%
    (:select 'datum :from 'feed :where (:= 'url '$1)))

(defun get-feed (url)
  (let ((result (get-feed% url)))
    (when result
      (values (read-from-string (caar result))))))

(defun create-item-table ()
  (execute (sql (:create-table item
                  ((hash :type string :primary-key t)
                   (datum :type string))))))

(defprepared item-recorded-p%
    (:select 'hash :from 'item :where (:= 'hash '$1)))

(defun item-recorded-p (hash)
  (not (null (item-recorded-p% hash))))

(defprepared record-item%
    (:insert-into 'item :set 'hash '$1 'datum '$2))

(defun record-item (hash item)
  (record-item% hash (prin1-to-string item))
  (values))

(defprepared get-item%
    (:select 'datum :from 'item :where (:= 'hash '$1)))

(defun get-item (hash)
  (let ((result (get-item% hash)))
    (when result
      (values (read-from-string (caar result))))))

(defun create-log-table ()
  (execute (sql (:create-table log
                  ((timestamp :type timestamp :primary-key t)
                   (imports :type string))))))

(defprepared log-imports%
    (:insert-into 'log :set 'timestamp '$1 'imports '$2))

(defun log-imports (imports)
  (let ((timestamp (get-universal-time)))
    (log-imports% (universal-time-to-timestamp timestamp)
                  (prin1-to-string imports))
    (sleep 1)
    timestamp))

(defprepared get-imports-1
    (:select 'imports :from 'log :where (:>= 'timestamp '$1)))

(defprepared get-imports-2
    (:select 'imports :from 'log :where (:and (:>= 'timestamp '$1)
                                              (:<= 'timestamp '$2))))

(defun get-imports (&optional (start 0) end)
  (mapcar (lambda (import)
            (read-from-string (car import)))
          (if end
              (get-imports-2 (universal-time-to-timestamp start)
                             (universal-time-to-timestamp end))
              (get-imports-1 (universal-time-to-timestamp start)))))
