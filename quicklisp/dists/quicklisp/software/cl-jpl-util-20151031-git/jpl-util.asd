;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem "jpl-util"
  :version "0.4"
  :maintainer "J.P. Larocque"
  :author "J.P. Larocque"
  :licence "ISC-style permissive"
  :description "Sundry utilities for J.P. Larocque."
  :components ((:file "jpl-util"
		:depends-on ("types"
			     "gensyms"
			     "accumulators"
			     "iterators"
			     "package"))
	       (:file "vector-delete"
		:depends-on ("types"
			     "jpl-util"
			     "package"))
	       (:file "gensyms"
		:depends-on ("package"))
	       (:file "types"
		:depends-on ("package"))
	       (:file "accumulators"
		:depends-on ("gensyms"
			     "types"
			     "package"))
	       (:file "iterators"
		:depends-on ("gensyms"
			     "package"))
	       (:file "subtypecase"
		:depends-on ("jpl-util"
			     "gensyms"
			     "package"))
	       (:file "option-clause"
		:depends-on ("jpl-util"
			     "alists-and-plists"
			     "package"))
	       (:file "alists-and-plists"
		:depends-on ("jpl-util"
			     "gensyms"
			     "accumulators"
			     "package"))
	       (:file "defclass-star"
		:depends-on ("jpl-util"
			     "alists-and-plists"
			     "package"))
	       (:file "order"
		:depends-on ("jpl-util"
			     "gensyms"
			     "package"))
               (:file "package"))
  :depends-on ())
