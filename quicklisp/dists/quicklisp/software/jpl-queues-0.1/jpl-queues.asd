;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem "jpl-queues"
  :version "0.1"
  :maintainer "J.P. Larocque"
  :author "J.P. Larocque"
  :licence "ISC-style permissive"
  :description "A few different kinds of queues, with optional
multithreading synchronization."
  :components ((:file "interface"
		:depends-on ("package"))
               (:file "bounded-fifo"
		:depends-on ("interface"
			     "package"))
               (:file "lossy-bounded-fifo"
		:depends-on ("bounded-fifo"
			     "interface"
			     "package"))
               (:file "unbounded-fifo"
		:depends-on ("interface"
			     "package"))
               (:file "unbounded-random"
		:depends-on ("interface"
			     "package"))
               (:file "synchronized"
		:depends-on ("interface"
			     "package"))
               (:file "package"))
  :depends-on ("bordeaux-threads" (:version "jpl-util" "0.2")))
