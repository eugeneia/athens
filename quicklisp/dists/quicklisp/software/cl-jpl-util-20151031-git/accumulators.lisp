(in-package #:jpl-util)

(defmacro accumulate-to-vector ((set-size/element-type accumulate)
				&body body)
  "Evaluates BODY, accumulating elements to a fresh vector, resulting
in the vector.  The number of elements to be accumulated must be
determined before accumulation begins.  The order of the elements of
the resulting vector is the order in which they are accumulated.

The symbols given by SET-SIZE/ELEMENT-TYPE and ACCUMULATE name
lexically-defined macros:

  (SET-SIZE/ELEMENT-TYPE SIZE &optional ELEMENT-TYPE)
    Must be evaluated once, before ACCUMULATE is evaluated for the
    first time.
    
    SIZE (evaluated) specifies the length of the resulting vector.
    
    ELEMENT-TYPE (evaluated) specifies the array element type of the
    resulting vector.  The default is T.  The result of accumulating
    an element not of type ELEMENT-TYPE is undefined.

  (ACCUMULATE ELEMENT)
    Accumulates ELEMENT (evaluated) to the vector.
    
    ACCUMULATE must never be evaluated more times than the size of the
    vector.  At the time BODY returns normally, ACCUMULATE must have
    been called exactly the number of times as the size of the vector.
    ACCUMULATE may be evaluated fewer times when BODY makes a
    non-local exit."
  (with-gensyms (out% out-pos%)
    `(let (,out%
	   (,out-pos% 0))
       (declare (type fixnum ,out-pos%))
       (macrolet ((,set-size/element-type (size &optional (type 't))
		    `(let ((size ,size)
			   (type ,type))
		       (check-type size array-dimension "an array dimension")
		       (setf ,',out% (make-array size :element-type type))))
		  (,accumulate (x)
		    `(progn
		       (setf (aref ,',out% ,',out-pos%) ,x)
		       (incf ,',out-pos%))))
	 ,@body)
       ,out%)))

(defmacro accumulate-to-dynamic-vector ((set-element-type accumulate)
					&body body)
  "Evaluates BODY, accumulating elements to a fresh vector, resulting
in the vector (which will be adjustable and have a fill-pointer).  The
number of elements to be accumulated does not have to be determined
before accumulation begins.  The order of the elements of the
resulting vector is the order in which they are accumulated.

The symbols given by SET-ELEMENT-TYPE and ACCUMULATE name
lexically-defined macros:

  (SET-ELEMENT-TYPE &optional ELEMENT-TYPE)
    Must be evaluated once, before ACCUMULATE is evaluated for the
    first time.
    
    ELEMENT-TYPE (evaluated) specifies the array element type of the
    resulting vector.  The default is T.  The result of accumulating
    an element not of type ELEMENT-TYPE is undefined.

  (ACCUMULATE ELEMENT)
    Accumulates ELEMENT (evaluated) to the vector.  ACCUMULATE may be
    evaluated any number of times."
  (with-gensyms (out%)
    `(let (,out%)
       (macrolet ((,set-element-type (&optional (type 't))
		    `(setf ,',out%
			   (make-array 0 :adjustable t :fill-pointer t
				       :element-type ,type)))
		  (,accumulate (x)
		    `(vector-push-extend ,x ,',out%)))
	 ,@body)
       ,out%)))

(defmacro accumulate-to-list ((accumulate) &body body)
  "Evaluates BODY, accumulating elements to a fresh list, resulting in
the list.  The order of the elements of the resulting list is the
order in which they are accumulated.

The symbol given by ACCUMULATE names a lexically-defined macro:

  (ACCUMULATE ELEMENT)
    Accumulates ELEMENT (evaluated) to the list.  ACCUMULATE may be
    evaluated any number of times."
  (with-gensyms (out+1% last-cell%)
    `(let* (;; The resulting list, but with a sentinel at the head.
	    (,out+1% (list nil))
	    (,last-cell% ,out+1%))
       (declare (ignorable ,last-cell%))
       (macrolet ((,accumulate (x)
		    `(setf ,',last-cell%
			   (setf (rest ,',last-cell%)
				 (list ,x)))))
	 ,@body)
       (rest ,out+1%))))

(defmacro accumulate-to-hash-table ((set-test/size accumulate)
				    &body body)
  "Evaluates BODY, accumulating key and value pairs to a fresh
hash-table, resulting in the hash-table.

The symbols given by SET-TEST/SIZE and ACCUMULATE name
lexically-defined macros:

  (SET-TEST/SIZE TEST &optional SIZE)
    Must be evaluated once, before ACCUMULATE is evaluated for the
    first time.

    TEST (evaluated) must be a designator for one of the test
    functions acceptable to MAKE-HASH-TABLE.

    If SIZE is specified and non-NIL, it is evaluated to produce a
    non-negative integer which specifies the expected number of unique
    entries to accumulate.  The actual number of entries may vary, but
    accumulation may be slower or use more memory than if the size had
    been correctly predicted.
  
  (ACCUMULATE KEY VALUE)
    Accumulates an entry of KEY and VALUE (both evaluated) to the
    hash-table, replacing any previous entry with the same KEY.
    ACCUMULATE may be evaluated any number of times."
  (with-gensyms (out%)
    `(let (,out%)
       (macrolet ((,set-test/size (test size)
		    `(setf ,',out% (make-hash-table :test ,test
						    ,@(unless (null size)
							`(:size ,size)))))
		  (,accumulate (key value)
		    `(setf (gethash ,key ,',out%) ,value)))
	 ,@body)
       ,out%)))
