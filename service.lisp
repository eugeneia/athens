(in-package :athens.service)

;; (apply 'athens-service (import-configuration ...))
(defun athens-service (&key database feed)
  (supervisor
   `((:log logger)
     (:timer timer)
     ,@(when feed
         `((:importer (feed-importer ,database))
           (:fetcher  (feed-fetcher  ,database ,@feed)))))))

(defun initialize-database (&key database &allow-other-keys)
  "Initialize database (needs to be called once to setup an empty
datatabase)."
  (with-database database
    (create-feed-table)
    (create-item-table)
    (values)))

(defun add-feed (url &key database &allow-other-keys)
  "Add feed at URL to archive."
  (with-database database
    (insert-feed (url-hash url) `(:source ,url :date 0))))

(defun remove-feed (feed-hash &key database &allow-other-keys)
  "Remove feed by FEED-HASH."
  (with-database database
    (delete-feed feed-hash)))
