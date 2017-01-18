;;;; Package definitions for Athens.

(defpackage athens.hash
  (:documentation "Hashing routines for feeds and feed items.")
  (:use :ironclad
        :cl
        :flexi-streams)
  (:shadow :null) ; Shadow IRONCLAD's NULL with CL's.
  (:export :url-hash))

(defpackage athens.store
  (:documentation "Persistent records.")
  (:use :cl
        :ccl
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
           :insert-item
           :get-item
           :get-items))

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
  (:export :html-widget-frontend
           :html-widget-feed
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

(defpackage athens.service
  (:use :cl
        :erlangen
        :erlangen-platform.log
        :erlangen-platform.supervisor
        :erlangen-platform.timer
        :erlangen-platform.server
        :athens.hash
        :athens.store
        :trivial-feed
        :drakma
        :flexi-streams
        :net.telent.date)
  (:export :athens-service
           :initialize-database
           :add-feed
           :remove-feed))

(defpackage athens.magnifier
  (:use :cl :parenscript)
  (:shadow :string))
