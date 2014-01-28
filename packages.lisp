;;;; Package definitions for ATHENS.

(defpackage athens.hash
  (:documentation "Hashing routines for feeds and feed items.")
  (:use :ironclad
        :cl
        :flexi-streams)
  (:shadow :null) ; Shadow IRONCLAD's NULL with CL's.
  (:export :feed-hash
           :feed-item-hash))

(defpackage athens.store
  (:documentation "Persistent logging and records.")
  (:use :cl
        :simple-date
        :postmodern)
  (:export :with-database
           :create-feed-table
           :feed-index
           :insert-feed
           :update-feed
           :get-feed
           :create-item-table
           :item-recorded-p
           :record-item
           :get-item
           :create-log-table
           :log-imports
           :get-imports))

(defpackage athens
  (:documentation "News archiver for syndication feeds.")
  (:use :cl
        :athens.hash
        :athens.store
        :configuration
        :trivial-feed
        :drakma
        :net.telent.date
        :flexi-streams)
  (:export :with-configuration
           :with-configuration-file
           :initialize-database
           :add-feed
           :update-archive))
