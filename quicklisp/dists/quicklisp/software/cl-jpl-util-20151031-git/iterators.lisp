(in-package #:jpl-util)

(defmacro with-list-iterator ((next size exhausted? list-form) &body body)
  "Evaluates BODY with an iterator on a list.

LIST-FORM is evaluated once to produce the list to iterate upon.  The
resulting list is never modified (unless from within BODY--the
consequences of which are undefined).

The symbols given by NEXT, SIZE, and EXHAUSTED? name lexically-defined
macros:

  (NEXT)
    Yields the next successive element of the list.  The consequences
    of evaluating (NEXT) more times than the length of the list is
    undefined.
  
  (SIZE)
    Evaluates to the length of the list.  The list is only traversed
    for only the first evaluation; the length is memoized.
  
  (EXHAUSTED?)
    Evaluates to a generalized boolean indicating whether the list has
    been exhausted by evaluations of (NEXT)."
  (with-gensyms (orig-list% size% remaining%)
    `(let* ((,orig-list% ,list-form)
	    (,size% nil)
	    (,remaining% ,orig-list%))
       (declare (type list ,orig-list% ,remaining%)
		(ignorable ,size%))
       (macrolet ((,next ()
		    '(pop ,remaining%))
		  (,size ()
		    '(if (null ,size%)
		         (setf ,size% (length ,orig-list%))
			 ,size%))
		  (,exhausted? ()
		    '(endp ,remaining%)))
	 ,@body))))

(defmacro with-vector-iterator ((next size exhausted? vector-form) &body body)
  "Evaluates BODY with an iterator on a vector.

VECTOR-FORM is evaluated once to produce the vector to iterate upon.
The resulting vector is never modified (unless from within BODY--the
consequences of which are undefined).  If the vector has a
fill-pointer, it is respected.

The symbols given by NEXT, SIZE, and EXHAUSTED? name lexically-defined
macros:

  (NEXT)
    Yields the next successive element of the vector.  The
    consequences of evaluating (NEXT) more times than the length of
    the vector is undefined.
  
  (SIZE)
    Evaluates to the length of the vector.
  
  (EXHAUSTED?)
    Evaluates to a generalized boolean indicating whether the vector
    has been exhausted by evaluations of (NEXT)."
  (with-gensyms (vector% pos%)
    `(let ((,vector% ,vector-form)
	   (,pos% 0))
       (declare (type vector ,vector%)
		(type fixnum ,pos%))
       (macrolet ((,next ()
		    '(prog1 (aref ,vector% ,pos%)
		       (incf ,pos%)))
		  (,size ()
		    '(length ,vector%))
		  (,exhausted? ()
		    '(= ,pos% (length ,vector%))))
	 ,@body))))

(defmacro with-sequence-iterator ((next size exhausted? sequence-form) &body body)
  "Evaluates BODY with an iterator on a SEQUENCE.

SEQUENCE-FORM is evaluated once to produce the sequence to iterate
upon.  The resulting sequence is never modified (unless from within
BODY--the consequences of which are undefined).  If the sequence is a
vector with a fill-pointer, the fill-pointer is respected.

The symbols given by NEXT, SIZE, and EXHAUSTED? name lexically-defined
macros:

  (NEXT)
    Yields the next successive element of the sequence.  The
    consequences of evaluating (NEXT) more times than the length of
    the sequence is undefined.
  
  (SIZE)
    Evaluates to the length of the sequence.
  
  (EXHAUSTED?)
    Evaluates to a generalized boolean indicating whether the sequence
    has been exhausted by evaluations of (NEXT)."
  (with-gensyms (seq%)
    `(let ((,seq% ,sequence-form))
       (etypecase ,seq%
	 (list (with-list-iterator (,next ,size ,exhausted? ,seq%)
		 ,@body))
	 (vector (with-vector-iterator (,next ,size ,exhausted? ,seq%)
		   ,@body))))))

(defmacro with-range-iterator ((next size exhausted? lower-form upper-form) &body body)
  "Evaluates BODY with an iterator on a range of integers.

LOWER-FORM and UPPER-FORM are evaluated once to produce the range of
integers, [LOWER,UPPER), to iterate upon.

The symbols given by NEXT, SIZE, and EXHAUSTED? name lexically-defined
macros:

  (NEXT)
    Yields the next successive element of the range.  The consequences
    of evaluating (NEXT) more times than the length of the range is
    undefined.
  
  (SIZE)
    Evaluates to the length of the range.
  
  (EXHAUSTED?)
    Evaluates to a generalized boolean indicating whether the range
    has been exhausted by evaluations of (NEXT)."
  (with-gensyms (counter% upper% size%)
    `(let ((,counter% ,lower-form)
	   (,upper% ,upper-form))
       (unless (and (typep ,counter% 'integer)
		    (typep ,upper% 'integer))
	 (error "LOWER-FORM and UPPER-FORM must have integer values, ~
                 but their actual values are ~S and ~S, respectively."
		,counter% ,upper%))
       (let ((,size% (- ,upper% ,counter%)))
	 (declare (type integer ,size%))
	 (when (minusp ,size%)
	   (error "LOWER-FORM must have a value lesser than or equal ~
                   to the value of UPPER-FORM, but their actual ~
                   values are ~D and ~D, respectively."
		  ,counter% ,upper%))
	 (macrolet ((,next ()
		      '(prog1 ,counter%
			 (incf ,counter%)))
		    (,size ()
		      ',size%)
		    (,exhausted? ()
		      '(= ,counter% ,upper%)))
	   ,@body)))))
