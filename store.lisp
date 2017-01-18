;;;; ATHENS.STORE: Persistent logging and records.

(in-package :athens.store)

(defun db-spec
    (&key host (port 5432) user password (name "athens") (use-ssl :yes)
     &allow-other-keys)
  `(,name ,user ,password ,host
    :port ,port :use-ssl ,use-ssl))

(defmacro with-database (db-configuration &body body
                         &aux (conf-sym (gensym "conf")))
  `(let ((,conf-sym ,db-configuration))
     (with-connection (apply #'db-spec ,conf-sym)
       (setf (stream-input-timeout
              #1=(cl-postgres::connection-socket *database*))
             #2=(getf ,conf-sym :timeout 10))
       (setf (stream-output-timeout #1#)
             #2#)
       ,@body)))

(defun create-feed-table ()
  (execute (sql (:create-table feed
                  ((hash :type string :primary-key t)
                   (datum :type string))))))

(defun feed-index ()
  (query (sql (:select 'hash :from 'feed))
         :column))

(defun insert-feed (hash feed)
  (query (sql (:insert-into 'feed :set 'hash '$1 'datum '$2))
         :none hash (prin1-to-string feed)))

(defun update-feed (hash feed)
  (query (sql (:update 'feed :set 'datum '$2 :where (:= 'hash '$1)))
         :none hash (prin1-to-string feed)))

(defun get-feed (hash)
  (read-from-string
   (query (sql (:select 'datum :from 'feed :where (:= 'hash '$1)))
          :single! hash)))

(defun delete-feed (hash)
  (query (sql (:delete-from 'feed :where (:= 'hash '$1)))
         :none hash))

(defun create-item-table ()
  (execute (sql (:create-table item
                  ((hash :type string :primary-key t)
                   (datum :type string)
                   (date :type timestamp))))))

(defun item-recorded-p (hash)
  (not (null (query (sql (:select 'hash :from 'item :where (:= 'hash '$1)))
                    :single hash))))

(defun insert-item (hash item)
  (query (sql (:insert-into 'item :set 'hash '$1 'datum '$2 'date '$3))
         :none
         hash (prin1-to-string item)
         (universal-time-to-timestamp (getf item :date))))

(defun get-item (hash)
  (read-from-string
   (query (sql (:select 'datum :from 'item :where (:= 'hash '$1)))
          :single! hash)))

(defprepared get-items-1
    (:select 'hash 'datum :from 'item :where (:>= 'date '$1)))

(defprepared get-items-2
    (:select 'hash 'datum :from 'item :where (:and (:>= 'date '$1)
                                                   (:<= 'date '$2))))

(defun get-items (&optional (start 0) end)
  (let ((items (if end
                   (get-items-2 (universal-time-to-timestamp start)
                                (universal-time-to-timestamp end))
                   (get-items-1 (universal-time-to-timestamp start)))))
    (loop for item in items do
         (setf #1=(second item) (read-from-string #1#)))
    items))
