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
                  ((hash :type string :primary-key t)
                   (datum :type string))))))

(defun feed-index ()
  (mapcar #'car (query (sql (:select 'hash :from 'feed)))))

(defprepared insert-feed%
    (:insert-into 'feed :set 'hash '$1 'datum '$2))

(defun insert-feed (hash feed)
  (insert-feed% hash (prin1-to-string feed))
  (values))

(defprepared update-feed%
    (:update 'feed :set 'datum '$2 :where (:= 'hash '$1)))

(defun update-feed (hash feed)
  (update-feed% hash (prin1-to-string feed))
  (values))

(defprepared get-feed%
    (:select 'datum :from 'feed :where (:= 'hash '$1)))

(defun get-feed (hash)
  (let ((result (get-feed% hash)))
    (when result
      (values (read-from-string (caar result))))))

(defprepared delete-feed%
    (:delete-from 'feed :where (:= 'hash '$1)))

(defun delete-feed (hash)
  (delete-feed% hash)
  (values))

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
