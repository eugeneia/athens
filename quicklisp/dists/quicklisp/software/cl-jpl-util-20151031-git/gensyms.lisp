(in-package #:jpl-util)

;;; Based on WITH-GENSYMS from "Practical Common Lisp" by Peter
;;; Seibel.
(defmacro with-gensyms ((&rest names) &body body)
  "Evaluates BODY with each variable name of NAMES bound to a unique,
fresh, uninterned symbol."
  `(let ,(loop for n in names
	       for new-name-prefix = (format nil "~A-" (symbol-name n))
	       collecting `(,n (gensym ,new-name-prefix)))
     ,@body))
