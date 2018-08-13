(in-package #:jpl-util)

;;; The missing types.

(deftype function-designator ()
  '(or function symbol))

(deftype extended-function-designator ()
  '(or function function-name))

(deftype function-name ()
  '(or symbol
       (and (cons (eql setf)
		  (cons symbol null)))))

(defun designated-function (extended-designator)
  (declare (type extended-function-designator extended-designator))
  (let ((definition (etypecase extended-designator
		      (function extended-designator)
		      (function-name (fdefinition extended-designator)))))
    (unless (functionp definition)
      (error "~S designates a ~S, not a function."
	     extended-designator (type-of definition)))
    definition))

(deftype pathname-designator ()
  '(or pathname string file-stream))

(deftype array-index ()
  `(integer 0 ,(1- array-dimension-limit)))

(deftype array-dimension ()
  `(integer 0 ,array-dimension-limit))

(deftype universal-time ()
  '(integer 0))

(deftype subsecond-universal-time ()
  '(rational 0))

(deftype format-control ()
  '(or string function))

(deftype class-designator ()
  '(or class symbol))

(defun designated-class (class-designator)
  (declare (type class-designator class-designator))
  (etypecase class-designator
    (class class-designator)
    (symbol (find-class class-designator))))
