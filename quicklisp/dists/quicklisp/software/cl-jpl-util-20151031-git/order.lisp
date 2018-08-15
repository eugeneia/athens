(in-package #:jpl-util)

(defmacro lesser? ((value1 value2 least-possible-value) &body body)
  "Evaluates to whether VALUE1 is less than VALUE2 when there is the
possibility that either are EQ to LEAST-POSSIBLE-VALUE.  One value is
always less than the other when it is EQ to LEAST-POSSIBLE-VALUE and
the other is not.  If the answer cannot be determined by comparing
VALUE1 and VALUE2 with LEAST-POSSIBLE-VALUE, then BODY is evaluated.

VALUE1, VALUE2, and LEAST-POSSIBLE-VALUE may or may not be evaluated,
and in any order.  BODY is evaluated only when neither values are EQ
to LEAST-POSSIBLE-VALUE."
  (with-gensyms (least-possible-value%)
    `(let ((,least-possible-value% ,least-possible-value))
       (and (not (eq ,value2 ,least-possible-value%))
	    (or (eq ,value1 ,least-possible-value%)
		(progn ,@body))))))

(defmacro composite-lesser? (&body clauses)
  "Returns a generalized boolean indicating whether one thing is less
than another thing according to the rules defined by CLAUSES.

Each thing (neither of which is explicitly expressed as an argument)
should conceptually be deconstructable to an equal number of
comparable parts.  Each part of both things is compared in parallel,
beginning with the first part of the things, followed by the next part
of the things, and so on.  One thing is less than another thing when
it has a part which is less than the other thing's corresponding part,
and all part pairs prior to the differing part pair compare as equal.

Each clause represents a part to test.  A clause is of the form:

  (TEST OBJ1 OBJ2)

OBJ1 and OBJ2 are expressions which produce parts of the first and
second thing, respectively.  TEST is evaluated to produce a function
of two arguments, which is applied to the two objects (in both orders)
to determine which one is lesser than the other, if any.

Additionally, the final clause may take the form:

  (:OTHERWISE &body OTHERWISE-BODY)

When this is so, and no differences are found by testing each pair of
parts, then the result--rather than NIL--is the evaluation of
OTHERWISE-BODY as an implicit progn.  This facilitates the comparison
of two things which have a variable number of parts; OTHERWISE-BODY
would test the tails of the parts which occur after the parts tested
for with ordinary clauses.

TEST, OBJ1, and OBJ2 for any given clause, and OTHERWISE-BODY, will be
evaluated at most once, but evaluation (and hence calling the function
expressed by TEST) may be short-circuited if testing an earlier part
proves that later part comparisons are irrelevant."
  (flet ((clause-type (clause)
	   (unless (and (proper-list? clause)
			(not (endp (rest clause))))
	     (error "Malformed clause (must be a proper, non-empty list): ~
                     ~S" clause))
	   (cond ((eq :otherwise (first clause))
		  :otherwise)
		 (t (unless (= 3 (length clause))
		      (error "Malformed clause (apparently an ordinary ~
                              clause, but not of the form (TEST OBJ1 OBJ2)): ~
                              ~S" clause))
		    :ordinary))))
    (cond ((endp clauses)
	   'nil)
	  ((and (endp (rest clauses))
		(eq :otherwise (clause-type (first clauses))))
	   `(progn ,@(rest (first clauses))))
	  (t (unless (eq :ordinary (clause-type (first clauses)))
	       (error "Only the last clause may be an :OTHERWISE clause."))
	     (destructuring-bind (test obj1 obj2) (first clauses)
	       (with-gensyms (test% obj1% obj2%)
		 `(let ((,test% ,test)
			(,obj1% ,obj1)
			(,obj2% ,obj2))
		    ,(let ((obj1<obj2 `(funcall ,test% ,obj1% ,obj2%))
			   (obj1>obj2 `(funcall ,test% ,obj2% ,obj1%)))
			  (if (endp (rest clauses))
			      ;; Avoid an unnecessary call to the test
			      ;; function.
			      obj1<obj2
			      `(or ,obj1<obj2
				   (and (not ,obj1>obj2)
					(composite-lesser?
					  ,@(rest clauses)))))))))))))

(defun symbol< (symbol1 symbol2)
  "Returns a boolean indicating whether SYMBOL1 is lesser than SYMBOL2
\(each are symbols) according to the order specified below.

A symbol is considered as the pair of its home package (or NIL if the
symbol is \"apparently uninterned\") and its name.  The two symbols
are compared as their two pairs.

When the package of SYMBOL1 is less than that of SYMBOL2, SYMBOL1 is
considered less than SYMBOL2, regardless of the symbol names--and
vice-versa.

When the packages of SYMBOL1 and SYMBOL2 are considered equal, SYMBOL1
is less than SYMBOL2 iff the name of SYMBOL1 is STRING< that of
SYMBOL2.

Packages are compared with PACKAGE<."
  (declare (type symbol symbol1 symbol2))
  (composite-lesser?
    (#'package< (symbol-package symbol1)
		(symbol-package symbol2))
    (#'string< (symbol-name symbol1)
	       (symbol-name symbol2))))

(let ((keyword-package (find-package '#:keyword)))
  (defun package< (package1 package2)
    "Returns a boolean indicating whether PACKAGE1 is lesser than
PACKAGE2 \(each are either PACKAGEs or NIL) according to the order
specified below.

The order of packages is: NIL, then any PACKAGE with no name, then the
KEYWORD package, then the sequence of all other packages ordered by
STRING< on their names."
    (declare (type (or package null) package1 package2))
    (lesser? (package1 package2 nil)
      (let ((name1 (package-name package1))
	    (name2 (package-name package2)))
	(lesser? (name1 name2 nil)
	  (lesser? (package1 package2 keyword-package)
	    (string< name1 name2)))))))

(defun test-order-pred (predicate elements)
  "Tests PREDICATE, an order predicate, with the given sequence of
elements.  Signals an error if an inconsistency is found.  Otherwise,
returns a vector of the items sorted in ascending order.

An order predicate is a function of two arguments which returns a
generalized boolean indicating whether the first argument is less than
the second.  It's the kind of function you would give to SORT."
  (labels ((item-cmp (x y)
	     (let ((lesser? (funcall predicate x y))
		   (greater? (funcall predicate y x)))
	       (cond ((and (not lesser?) (not greater?))
		      0)
		     ((and lesser? (not greater?))
		      -1)
		     ((and (not lesser?) greater?)
		      1)
		     (t (error "Order predicate is inconsistent with ~
                                  respect to X = ~S and Y = ~S.  Predicate ~
                                  indicates that X is ~:[not ~;~]less than Y ~
                                  and that Y is ~:[not ~;~]less than X."
			       x y lesser? greater?)))))
	   (item< (x y)
	     (minusp (item-cmp x y)))
	   (item<= (x y)
	     (not (item> x y)))
	   (item= (x y)
	     (zerop (item-cmp x y)))
	   (item> (x y)
	     (plusp (item-cmp x y)))
	   (item>= (x y)
	     (not (item< x y))))
    (loop with elements-v = (make-array (length elements)
					:initial-contents elements)
	  with sorted = (nsort elements-v #'item<)
	  for idx1 below (length sorted)
	  for elt1 = (elt sorted idx1)
	  do (loop for idx2 below (length sorted)
		   for elt2 = (elt sorted idx2)
		   do (cond ((< idx1 idx2)
			     (assert (item<= elt1 elt2) ()
				     "Order predicate should indicate that ~
                                      ~S is less than or equal to ~S, but it ~
                                      does not." elt1 elt2))
			    ((= idx1 idx2)
			     (assert (item= elt1 elt2) ()
				     "Order predicate should indicate that ~
                                      ~S is equal to ~S, but it does not."
				     elt1 elt2))
			    ((> idx1 idx2)
			     (assert (item>= elt1 elt2) ()
				     "Order predicate should indicate that ~
                                      ~S is greater than or equal to ~S, but ~
                                      it does not." elt1 elt2))))
	  finally (return sorted))))
