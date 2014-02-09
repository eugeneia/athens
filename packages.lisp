;;;; Package definitions for Athens.

(defpackage athens.hash
  (:documentation "Hashing routines for feeds and feed items.")
  (:use :ironclad
        :cl
        :flexi-streams)
  (:shadow :null) ; Shadow IRONCLAD's NULL with CL's.
  (:export :url-hash))

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
           :delete-feed
           :create-item-table
           :item-recorded-p
           :record-item
           :get-item
           :get-items
           :create-log-table
           :log-imports
           :get-imports
           :create-global-date-table
           :get-global-date
           :update-global-date))

(defpackage athens.widgets
  (:documentation
   "HTML widgets for Athens.")
  (:use :cl
        :macro-html
        :macro-html.widgets
        :named-readtables
        :net.telent.date
        :pretty-string)
  (:shadow :map :time)
  (:export :html-widget-feed
           :html-widget-item
           :html-widget-news))

(defpackage athens.restful-responder
  (:documentation
   "RESTful resource responder.")
  (:use :cl
	:trivial-utf-8
	:httpd0.responses
        :jsown
        :athens.store
        :athens.widgets)
  (:export :make-athens-responder))

(defpackage athens
  (:documentation "News archiver for syndication feeds.")
  (:use :cl
        :athens.hash
        :athens.store
        :athens.restful-responder
        :configuration
        :trivial-feed
        :drakma
        :net.telent.date
        :flexi-streams
        :httpd0)
  (:export :with-configuration
           :with-configuration-file
           :initialize-database
           :add-feed
           :remove-feed
           :update-archive
           :update-archive-periodically
           :archive-log
           :make-server))
