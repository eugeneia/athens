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
        :athens.hash
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
        :named-readtables)
  (:import-from :httpd0.responses :universal-time-to-http-date)
  (:shadow :map :time)
  (:export :html-widget-frontend
           :html-widget-feed
           :html-widget-item
           :html-widget-news))

(defpackage athens.restful-responder
  (:documentation
   "RESTful resource responder.")
  (:use :cl
        :ccl
        :erlangen-platform.log
        :erlangen-platform.socket-server
        :athens.store
        :athens.widgets
        :flexi-streams
        :httpd0
	:httpd0.responses
        :httpd0.router
        :jonathan)
  (:import-from :parenscript :ps-compile-file)
  (:import-from :asdf :system-relative-pathname)
  (:shadow :make-external-format)
  (:export :athens-responder))

(defpackage athens.service
  (:use :cl
        :erlangen
        :erlangen-platform.log
        :erlangen-platform.supervisor
        :erlangen-platform.timer
        :erlangen-platform.server
        :erlangen-platform.socket-server
        :athens.hash
        :athens.store
        :athens.restful-responder
        :trivial-feed
        :drakma
        :flexi-streams
        :sanitize)
  (:shadow :call)
  (:import-from :httpd0.responses :universal-time-to-http-date)
  (:export :athens-service
           :initialize-database
           :add-feed
           :remove-feed))

(defpackage athens.magnifier
  (:use :cl :parenscript)
  (:shadow :string))
