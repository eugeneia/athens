(in-package :athens.service)

;; (apply 'athens-service (import-configuration ...))
(defun athens-service (&key database feed server)
  (supervisor
   `((:log logger :register-p t)
     (:athens (supervisor
               ((:importer (feed-importer ,database) :register-p t)
                (:scanner (supervisor
                           ((:fetch-timer (timer :log :log) :register-p t)
                            (:fetcher (feed-fetcher  ,database ,@feed)))
                           :strategy one-for-all
                           :log :log))
                (:service (socket-server
                           :responder athens-responder
                           :extra-arguments (,database)
                           :supervisor-arguments (:intensity 10000 :log :log)
                           ,@server)))
               :log :log)))))

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
    (insert-feed url)))

(defun remove-feed (feed-hash &key database &allow-other-keys)
  "Remove feed by FEED-HASH."
  (with-database database
    (delete-feed feed-hash)))
