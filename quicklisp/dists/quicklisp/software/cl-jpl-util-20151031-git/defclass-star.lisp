(in-package #:jpl-util)

(defmacro defclass* (class-name (&rest superclass-names)
		     (&rest slot-specifiers) &rest class-options)
  "Defines a class just like DEFCLASS does, but adds reasonable
default options to each slot.

:INITARG is defined, with the slot name as a keyword.

:INITFORM is defined, with an ERROR form that signals a SIMPLE-ERROR
with a generic message, \"Must supply [INITARG].\"

A reader, writer, or accessor is defined, with the same name as the
slot.  By default, a reader is defined; you may override the selection
with the special slot option JPL-UTIL:ACCESSOR-TYPE, with a value
of :READER, :WRITER, or :ACCESSOR.  The name of the reader, writer, or
accessor may be overridden with the special slot option
JPL-UTIL:ACCESSOR-NAME.  If either value of JPL-UTIL:ACCESSOR-TYPE or
JPL-UTIL:ACCESSOR-NAME are NIL, then no reader, writer, or accessor is
defined.

Any default slot option may be overridden by explicitly specifying a
value for that slot option.  For example, if you don't like the
default value for :INITFORM, you may specify :INITFORM NIL.

Default slot option may be omitted by giving the special slot option
JPL-UTIL:OMIT-OPTIONS with a list of default slot option indicators to
suppress.  For example, if you don't want an :INITFORM option, you may
specify JPL-UTIL:OMIT-OPTIONS (:INITFORM)."
  `(defclass ,class-name (,@superclass-names)
     ,(loop with none = (gensym)
	    for (slot-name . slot-options) in slot-specifiers
	    for accessor-name = (getf slot-options 'accessor-name slot-name)
	    for accessor-type = (getf slot-options 'accessor-type :reader)
	    for initarg = (let ((x (getf slot-options :initarg none)))
			    (if (not (eq x none)) x
				;; Avoid unnecessary pollution of KEYWORD.
				(intern (symbol-name slot-name) '#:keyword)))
	    for omit-options = (getf slot-options 'omit-options)
	    for default-options
	    = `(:initarg ,initarg
		:initform (error ,(format nil "Must supply ~S." initarg))
		,@(if (or (null accessor-name)
			  (null accessor-type))
		      '()
		      `(,accessor-type ,accessor-name)))
	    for effective-options
	    = (alist->plist
	       (append
		(remove-if (curry-right #'member
					'(accessor-name accessor-type
					  omit-options))
			   (plist->alist slot-options)
			   :key #'car)
		(remove-if (curry-right #'member omit-options)
			   (plist->alist default-options)
			   :key #'car)))
	    collecting `(,slot-name ,@effective-options))
     ,@class-options))
