(in-package #:jpl-util)

(defmacro subtypecase (key-form &body clauses)
  "Evaluates the code associated with the first clause which lists a
type which KEY-FORM (which is evaluated) a recognizable subtype of.
If there is no match, NIL is returned.  SUBTYPECASE is to SUBTYPEP as
TYPECASE is to TYPEP.

The syntax of clauses, including the syntax of an optional trailing
otherwise-clause, is the same as in TYPECASE."
  (let ((clauses (loop with length = (length clauses)
		       for (clause-type . clause-body) in clauses
		       for i from 0
		       for last-p = (= i (1- length))
		       for real-type = (if (and last-p
						(eq 'otherwise clause-type))
					   t
					   clause-type)
		       collecting `(,real-type ,@clause-body))))
    (with-gensyms (block% key%)
      `(block ,block%
	 (let ((,key% ,key-form))
	   ,@(loop for (clause-type . clause-body) in clauses
		   collecting `(when (subtypep ,key% ',clause-type)
				 (return-from ,block%
				   (progn ,@clause-body)))))))))

(defmacro esubtypecase (key-form &body clauses)
  "As with SUBTYPECASE, but signals SUBTYPE-ERROR if no clause
matches.  No otherwise clause is allowed.  Analogous to ETYPECASE."
  (with-gensyms (key%)
    `(let ((,key% ,key-form))
       (subtypecase ,key%
	 ,@clauses
	 (otherwise
	  (error 'subtype-error :type ,key%
		 :expected-supertype '(or ,@(map 'list #'first clauses))))))))

(defmacro csubtypecase (key-place &body clauses)
  "As with SUBTYPECASE, but signals a correctable error of type
SUBTYPE-ERROR if no clause matches.  The STORE-VALUE restart can be
used to correct the error.  No otherwise clause is allowed.  Analogous
to CTYPECASE."
  (with-gensyms (key% block% loop%)
    `(block ,block%
       (let ((,key% ,key-place))
	 (tagbody
	    ,loop%
	    (return-from ,block%
	      (subtypecase ,key%
		,@clauses
		(otherwise
		 (restart-case (error 'subtype-error :type ,key%
				      :expected-supertype
				      '(or ,@(map 'list #'first clauses)))
		   (store-value (new-value)
		     :report (lambda (s)
			       (format s "Supply a new value for ~S."
				       ',key-place))
		     :interactive read-new-value
		     (setf ,key% new-value
			   ,key-place ,key%)
		     (go ,loop%)))))))))))

(define-condition subtype-error (error)
  ((type :initarg :type :initform (error "Must supply :TYPE.")
	 :reader subtype-error-type)
   (expected-supertype :initarg :expected-supertype
		       :initform (error "Must supply :EXPECTED-SUPERTYPE.")
		       :reader subtype-error-expected-supertype))
  (:documentation "TYPE is not a subtype of EXPECTED-SUPERTYPE.")
  (:report (lambda (c stream)
	     (format stream "~S is not a subtype of ~S"
		     (subtype-error-type c)
		     (subtype-error-expected-supertype c)))))
