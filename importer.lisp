(in-package :athens.service)

(defun import-item (feed-hash item
                    &aux (link (getf item :link))
                         (item-hash (and link (url-hash link)))
                         (item (list* :feed feed-hash item)))
  (if link
      (unless (item-recorded-p item-hash)
        (insert-item item-hash item)
        (write-log `(:recorded ,item)))
      (write-log `(:ignored-item ,item))))

(defun import-feed (feed from state)
  (declare (ignore from state))
  (destructuring-bind (&key source items link date title description
                       &aux (feed-hash (url-hash source)))
      feed
    (write-log `(:updating ,feed-hash))
    (loop for item in items do (import-item feed-hash item))
    (update-feed feed-hash (list :source source
                                 :link link
                                 :date date
                                 :title title
                                 :description (clean description +basic+))))
  (values nil 'no-value))

(defun feed-importer (db-connection)
  (with-database db-connection
    (server 'import-feed)))
