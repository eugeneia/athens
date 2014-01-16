;;;; Package definitions for ATHENS.

(defpackage athens.hash
  (:documentation "Hashing routines for feeds and feed items.")
  (:use :ironclad :cl :trivial-utf-8)
  (:shadow :null) ; Shadow IRONCLAD's NULL with CL's.
  (:export :feed-hash
           :feed-item-hash))

(defpackage athens.io
  (:documentation "Persistent logging and records I/O.")
  (:use :cl :athens.hash)
  (:export :open-store
           :with-store
           :log-item
           :log-import
           :log-failure
           :record-feed
           :purge-feed
           :read-item
           :read-feed
           :read-imports
           :read-failures))

(defpackage athens.import
  (:documentation "Syndication feed importer.")
  (:use :cl :athens.io :drakma)
  (:export :import-feed))

(defpackage athens
  (:documentation "News archiver for syndication feeds.")
  (:use :cl :athens.io :athens.import)
  (:export :add-feed
           :remove-feed
           :update-archive
           :archive-report
           :start-archive-daemon
           :stop-archive-daemon))
