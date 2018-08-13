(in-package #:jpl-util)


(defmacro lambda* (args &body body)
  "Creates a function.  Takes an extended, rather than an ordinary, lambda list."
  ;; TODO: Consider optimizing for the case that args can be expressed
  ;; in an ordinary lambda.
  (let ((args-sym (gensym)))
    `(function (lambda (&rest ,args-sym)
       (destructuring-bind ,args ,args-sym
	 ,@body)))))


(defgeneric copy-object (obj)
  (:documentation "Recursively copies an object.  The output is
guaranteed to not share structure with the input.  Patterns of shared
structure may not be replicated in the output.  If the input has
reference cycles, this function may not return or signal a
condition."))

(defmethod copy-object ((cons-obj cons))
  (cons (copy-object (car cons-obj))
	(copy-object (cdr cons-obj))))

(defmethod copy-object ((seq sequence))
  (map (type-of seq) #'copy-object seq))

;;; Presumably immutable objects.
(defmethod copy-object ((obj number))
  obj)
(defmethod copy-object ((obj null))
  obj)
(defmethod copy-object ((obj symbol))
  obj)
(defmethod copy-object ((obj character))
  obj)


(defun combine-elements (sequence predicate combination-fn)
  "Conditionally combines the elements of sequence to form a new list.
When two adjacent elements each individually pass the predicate, the
combination-fn is called on both of them, in the order that they
appear in the sequence, to form a new replacement item.  That
replacement item is subject to further combination."
  ;; Example: combines strings.  Note that sublists are not traversed.
  ;; (combine-elements '("f" "o" "o" ("bar" "forever") "!")
  ;;                   (lambda (e) (typep e 'string))
  ;;                   (lambda (e1 e2) (concatenate 'string e1 e2)))
  ;; => ("foo" ("bar" "forever") "!")
  (reduce (lambda (elt elt-list)
	    (if (and (funcall predicate elt)
		     (funcall predicate (first elt-list)))
		(list* (funcall combination-fn elt (first elt-list))
		       (rest elt-list))
		(list* elt elt-list)))
	  sequence :initial-value '() :from-end t))


(defun eof-p (stream)
  (null (peek-char nil stream nil)))

(defun map-lines (func stream)
  (loop for line = (read-line stream nil)
	until (null line)
	collecting (funcall func line)))

(defun read-lines (stream)
  (map-lines #'identity stream))

(defun zip (&rest lists)
  (zip* lists))

(defun zip* (lists &key (pad nil pad-p))
  ;; FIXME: when you find a good generalized iteration/accumulation
  ;; sequences library, adapt this code to use it.
  "Combines the elements of each list of LISTS, returning a fresh list
where the element at position n is a fresh list of corresponding
elements from the input lists, each at position n.  To put it another
way, if you think of the input lists as a matrix, this returns the
transpose of the matrix.

If PAD is specified and the lengths of the input lists are not equal,
then the lists will be implicitly padded with PAD at the end, as if
all the lists were of the same length as the longest list."
  (loop with tails = (copy-list lists)
	initially (when (endp tails)
		    (return '()))
	while (if pad-p
		  (notevery #'endp tails)
		  (notany #'endp tails))
	collecting (loop for tails-tail on tails
			 collecting (if (endp (first tails-tail))
					pad
					(pop (first tails-tail))))))

(defun partition-set (set &key (test #'eql) (key #'identity) ordered-p)
  "Groups elements in SET (a list) according to whether they are equal
according to TEST.  The result is a set of sets, represented as a
fresh list of fresh lists: each contained set contains a member of
SET, and every member of each such set will be equal to every other
member of that same set (according to TEST).

When ORDERED-P is true, the order of the returned list of sets is the
order in which the first element of each set is seen in SET.
Furthermore, the order of each element in each returned set is the
order in which that element was seen in SET.  Preserving the order
with ORDERED-P incurs a small constant-factor speed penalty.

TEST must be consistent.  For any elements x y and z in SET, when x
and y are equal according to TEST, and y and z are equal according to
TEST, x and z must also be equal according to TEST.

The result of applying KEY each element is used for TEST.

For pairwise partitioning, see PARTITION-LIST."
  (let ((sets '())
	(member-key (compose key #'first)))
    (dolist (item set)
      (let ((sets-tail (member (funcall key item) sets
			       :test test :key member-key)))
	(when (null sets-tail)
	  (setf sets-tail (push '() sets)))
	(push item (first sets-tail))))
    (when ordered-p
      (loop for sets-tail on sets
	    doing (setf (first sets-tail) (nreverse (first sets-tail))))
      (setf sets (nreverse sets)))
    sets))

(defun partition-list (list &key (test #'eql) (key #'identity))
  "Partitions LIST into a list of sublists according to whether
adjacent elements pass the test.  Adjacent elements which pass the
test are placed into the same sublist.

Two adjacent elements A and B (in that order) pass the test when
applying TEST to the key of A and the key of B results in a true
generalized boolean value.  The key of an element is produced by
applying KEY to the element.

The result is a list of sublists.  Every adjacent pair of elements A
and B in a resulting sublist pass the test:

  (... (... A B ...) ...)     (funcall TEST A* B*) returns true

Every pair of elements A and B, where A ends a resulting sublist and B
begins the subsequent sublist, do not pass the test:
  
  (... (... A) (B ...) ...)   (funcall TEST A* B*) returns false

The first element of the first resulting sublist is the first element
of LIST.  The last element of the last resulting sublist is the last
element of LIST.  If LIST is empty, then the resulting list of
sublists is also empty.

The returned list of sublists, as well as each of its sublists, is
freshly-allocated.

For set partitioning, see PARTITION-SET."
  ;; This algorithm is (to me) more clearly expressed with
  ;; MAP-ADJACENT-PAIRS and list-based queues, but unfortunately
  ;; jpl-queues depends on jpl-util, and I'd hate to bring in
  ;; rsm.queue.
  (loop with remaining = list
	until (endp remaining)
	collecting (loop for e1 = (pop remaining)
			 for e2 = (first remaining)
			 collecting e1
			 when (or (endp remaining)
				  (not (funcall test
						(funcall key e1)
						(funcall key e2))))
			 doing (loop-finish))))

(defun group-by-n (list n)
  "Groups elements in the given LIST into lists that are N elements in
length.  The number of elements in list must be a multiple of N."
  (check-type n (integer 1) "positive integer")
  (if (endp list)
      '()
      (let ((group (subseq list 0 n)))
	(list* group (group-by-n (nthcdr n list) n)))))


(defun compose (&rest functions)
  "Returns the composition of a sequence of functions (designated by
FUNCTIONS), yielding a new function.  The new function passes all its
arguments to the last given function.  Each other given function gets
called, from last to first, with the values of the call of its
successive function.  The new function returns all the values returned
by the first given function.

For single-argument functions a and b, and single-value functions b
and c:
  (FUNCALL (COMPOSE a b c) x y z) == (a (b (c x y z)))
For any function f:
  (COMPOSE f) == f
For any values x, y, and z:
  COMPOSE == VALUES
  (FUNCALL (COMPOSE)) => ; no value
  (FUNCALL (COMPOSE) x) => x
  (FUNCALL (COMPOSE) x y z) => x y z"
  (flet ((compose-2 (f1 f2)
	   (lambda (&rest args)
	     (multiple-value-call f1 (apply f2 args)))))
    (reduce #'compose-2 functions :initial-value #'values :from-end t)))

;;; Implementation based on public domain example written by Peter
;;; Scott at: http://www.cliki.net/COMPOSE
(define-compiler-macro compose (&rest function-exprs)
  (flet ((function-call-expr (func-expr args-expr)
	   "Given an expression of a function designator, and an
expression of multiple values to be used as multiple arguments to that
function, returns an expression of a call to the designated function
with the designated arguments."
	   `(multiple-value-call ,func-expr ,args-expr)))
    (with-gensyms (args%)
      `(lambda (&rest ,args%)
	 ,(reduce #'function-call-expr function-exprs
		  :initial-value `(values-list ,args%) :from-end t)))))

(defun compose-1v (&rest functions)
  "Like COMPOSE, but restricted to (and optimized for) functions that
accept only one argument.  The resulting function accepts only one
argument, and only the primary return value of each of the latter
given functions is given to their preceding functions.  The resulting
function returns all the values returned by the first given function.

For single-argument functions a and b, and single-value functions b
and c:
  (FUNCALL (COMPOSE-1V a b c) x) == (a (b (c x)))
For any function f of one argument:
  (COMPOSE f) == f
For any value x:
  COMPOSE == IDENTITY
  (FUNCALL (COMPOSE) x) => x"
  (flet ((compose-2 (f1 f2)
	   (lambda (arg)
	     (funcall f1 (funcall f2 arg)))))
    (reduce #'compose-2 functions :initial-value #'identity :from-end t)))

;;; Implementation based on public domain example written by Peter
;;; Scott at: http://www.cliki.net/COMPOSE
(define-compiler-macro compose-1v (&rest function-exprs)
  (flet ((function-call-expr (func-expr arg-expr)
	   "Given an expression of a function designator, and an
expression whose primary value is to be used a single argument to that
function, returns an expression of a call to the designated function
with the designated argument."
	   ;; Opting not to rewrite as in:
	   ;; 
	   ;;   `(,(second func-expr) ,arg-expr)
	   ;; 
	   ;; On SBCL 1.0.18, there is no difference in performance,
	   ;; and this is an order of magnitude simpler than dealing
	   ;; with all the edge-cases of macros and special operators
	   ;; and such.
	   `(funcall ,func-expr ,arg-expr)))
    (with-gensyms (arg%)
      `(lambda (,arg%)
	 ,(reduce #'function-call-expr function-exprs
		  :initial-value arg% :from-end t)))))

(defun nth-arg (n)
  "Returns a function which returns its Nth argument.  Useful when
composing a function with a function that returns multiple values."
  (lambda (&rest args)
    (nth n args)))

(defun curry-left (function &rest left-args)
  "Partially applies each of LEFT-ARGS to FUNCTION, yielding a new
function of the rest of the arguments accepted by FUNCTION.

  (CURRY-LEFT f) == f ; Equivalent, but not identical.
  (FUNCALL (CURRY-LEFT f a)) == (FUNCALL f a)
  (FUNCALL (CURRY-LEFT f a b c) d e f) == (FUNCALL f a b c d e f)"
  (lambda (&rest args)
    (apply function (append left-args args))))

(defun curry-right (function &rest right-args)
  "Partially applies each of RIGHT-ARGS as the last arguments to
function, in left-to-right order, yielding a new function of the
rest of the arguments accepted by FUNCTION.

  (CURRY-RIGHT f) == f ; Equivalent, but not identical.
  (FUNCALL (CURRY-RIGHT f a)) == (FUNCALL f a)
  (FUNCALL (CURRY-RIGHT f a b)) == (FUNCALL f a b)
  (FUNCALL (CURRY-RIGHT f d e f) a b c) == (FUNCALL f a b c d e f)"
  (lambda (&rest args)
    (apply function (append args right-args))))


(defun sort (sequence predicate &key (key nil key-spec-p))
  (apply #'cl:sort (copy-seq sequence) predicate
	 (if key-spec-p
	     (list :key key)
	     '())))
(defun stable-sort (sequence predicate &key (key nil key-spec-p))
  (apply #'cl:stable-sort (copy-seq sequence) predicate
	 (if key-spec-p
	     (list :key key)
	     '())))
(defun nsort (sequence predicate &key (key nil key-spec-p))
  (apply #'cl:sort sequence predicate
	 (if key-spec-p
	     (list :key key)
	     '())))
(defun nstable-sort (sequence predicate &key (key nil key-spec-p))
  (apply #'cl:stable-sort sequence predicate
	 (if key-spec-p
	     (list :key key)
	     '())))

(defun coerce-boolean (b)
  "Normalizes the given generalized boolean to a boolean--that is,
non-NIL maps to T, and NIL maps to NIL."
  (if b t nil))


(declaim (inline check-bounding-indices))
(defun check-bounding-indices (start-req end-req start-actual end-actual length
				&key (sequence-desc "sequence"))
  (declare (type array-dimension start-req start-actual end-actual length)
	   (type (or array-dimension null) end-req))
  (unless (<= 0 start-actual end-actual length)
    (error "The bounding index designators ~S and ~S are bad for a ~A of length ~D."
	   start-req end-req sequence-desc length)))

(macrolet ((define-substring-fn (function-name compare-function-name
				 comparison-start1-expr comparison-end1-expr)
	     `(defun ,function-name (string substring &key
				     (start1 0) end1 (start2 0) end2)
	       (let* ((end1-e (if (null end1) (length string) end1))
		      (string-length (- end1-e start1))
		      (end2-e (if (null end2) (length substring) end2))
		      (substring-length (- end2-e start2)))
		 (check-bounding-indices start1 end1 start1 end1-e (length string)
					 :sequence-desc "string")
		 (check-bounding-indices start2 end2 start2 end2-e (length substring)
					 :sequence-desc "string")
		 (and (>= string-length substring-length)
		      (,compare-function-name
		       string substring
		       :start1 ,comparison-start1-expr :end1 ,comparison-end1-expr
		       :start2 start2 :end2 end2-e))))))
  (define-substring-fn string-begin= string= start1 (+ start1 substring-length))
  (define-substring-fn string-end= string= (- end1-e substring-length) end1-e)
  (define-substring-fn string-begin-equal string-equal start1 (+ start1 substring-length))
  (define-substring-fn string-end-equal string-equal (- end1-e substring-length) end1-e))

(defun cond-replace (sequence1 test-sequence new-sequence &key
		     (test 'eql) (key 'identity)
		     (sequence1-start 0) sequence1-end
		     (test-sequence-start 0) test-sequence-end
		     (new-sequence-start 0) new-sequence-end)
  ;; In principle this function should be faster than techniques like
  ;; displacing strings and testing them, and concatenating.  But
  ;; whether it's really faster is completely unknown.  It was fun to
  ;; write, and it's surely filled to the brim with terrible bugs.
  ;; Enjoy!
  "Tests whether SEQUENCE1 is equal to TEST-SEQUENCE according to TEST
and KEY.  When they are equal, destructively replaces SEQUENCE1 with
the contents of NEW-SEQUENCE, and returns the modified SEQUENCE1 and T.
Otherwise, returns SEQUENCE1 (unmodified) and NIL.

If SEQUENCE1 and TEST-SEQUENCE match, and if the lengths of
TEST-SEQUENCE and NEW-SEQUENCE differ, SEQUENCE1 will be resized
accordingly.

When SEQUENCE1 is a vector and doesn't need to be adjusted to a
different size, or is actually adjustable, or has a fill pointer which
either needs to be decreased or needs to be increased within the
dimensions of the vector, then the resulting sequence is guaranteed to
be identical to SEQUENCE1.  Otherwise, the resulting sequence may
differ from SEQUENCE1 and should replace it.

SEQUENCE1, TEST-SEQUENCE, and NEW-SEQUENCE are bounded by
SEQUENCE1-START and SEQUENCE1-END, by TEST-SEQUENCE-START and
TEST-SEQUENCE-END, and by NEW-SEQUENCE-START and NEW-SEQUENCE-END,
respectively.

Only guaranteed to be efficient on vectors."
  (declare (type sequence sequence1 test-sequence new-sequence)
	   (type extended-function-designator test key)
	   (type array-dimension
		 sequence1-start test-sequence-start new-sequence-start)
	   (type (or array-dimension null)
		 sequence1-end test-sequence-end new-sequence-end))
  (labels (;; This function is obviously hard-coded for only two
	   ;; sequences (to avoid APPLY), and otherwise has too
	   ;; unpalatable of an interface to be made public (no
	   ;; result; end designator required).  If such is desired,
	   ;; though, it shouldn't be too hard to make it general.
	   (map-nil/2/bounds (function sequence-designator-1 sequence-designator-2)
	     ;; A sequence designator is (sequence start end).  All
	     ;; lengths must be the same.
	     (labels ((desig-length (sequence-designator)
			(destructuring-bind (sequence start end) sequence-designator
			  (declare (ignore sequence))
			  (- end start)))
		      ;; A next-func always returns the next item in
		      ;; its subsequence.  It takes the logical index
		      ;; into the subsequence.  Calls to the next-func
		      ;; must be with indices which sequentially
		      ;; increase by 1 (some may ignore the index).
		      (next-func (sequence-designator)
			(destructuring-bind (sequence start end) sequence-designator
			  (declare (ignore end))
			  (etypecase sequence
			    (list (let ((list (nthcdr start sequence)))
				    (lambda (index)
				      (declare (ignore index))
				      (pop list))))
			    (vector (lambda (index)
				      (aref sequence (+ start index))))))))
	       (let ((length (desig-length sequence-designator-1))
		     (next-func-1 (next-func sequence-designator-1))
		     (next-func-2 (next-func sequence-designator-2)))
		 (assert (= length (desig-length sequence-designator-2)))
		 (loop for i from 0 below length
		       doing (funcall function
				      (funcall next-func-1 i)
				      (funcall next-func-2 i)))))))
    (let* ((test (designated-function test))
	   (key (designated-function key))
	   (key-identity? (eq key #'identity))
	   (string-function (when (and key-identity?
				       (stringp sequence1)
				       (stringp test-sequence))
			      (cond ((or (eq test #'equal)
					 (eq test #'char=))
				     #'string=)
				    ((or (eq test #'equalp)
					 (eq test #'char-equal))
				     #'string-equal))))
	   (sequence1-end-e (if (null sequence1-end) (length sequence1) sequence1-end))
	   (sequence1-length (- sequence1-end-e sequence1-start))
	   (test-sequence-end-e (if (null test-sequence-end) (length test-sequence) test-sequence-end))
	   (test-sequence-length (- test-sequence-end-e test-sequence-start))
	   (new-sequence-end-e (if (null new-sequence-end) (length new-sequence) new-sequence-end))
	   (new-sequence-length (- new-sequence-end-e new-sequence-start)))
      (check-bounding-indices sequence1-start sequence1-end
			      sequence1-start sequence1-end-e
			      (length sequence1))
      (check-bounding-indices test-sequence-start test-sequence-end
			      test-sequence-start test-sequence-end-e
			      (length test-sequence))
      (check-bounding-indices new-sequence-start new-sequence-end
			      new-sequence-start new-sequence-end-e
			      (length new-sequence))
      ;;(format t "~&SEQUENCE1: [~D,~D) -- ~D" sequence1-start sequence1-end-e sequence1-length)
      ;;(format t "~&TEST-SEQUENCE: [~D,~D) -- ~D" test-sequence-start test-sequence-end-e test-sequence-length)
      ;;(format t "~&NEW-SEQUENCE: [~D,~D) -- ~D" new-sequence-start new-sequence-end-e new-sequence-length)
      (unless (and (= sequence1-length test-sequence-length)
		   (if (not (null string-function))
		       (funcall string-function sequence1 test-sequence
				:start1 sequence1-start :end1 sequence1-end-e
				:start2 test-sequence-start :end2 test-sequence-end-e)
		       (block compare-result
			 (map-nil/2/bounds (if key-identity?
					       (lambda (item1 item2)
						 (unless (funcall test item1 item2)
						   (return-from compare-result nil)))
					       (lambda (item1 item2)
						 (unless (funcall test
								  (funcall key item1)
								  (funcall key item2))
						   (return-from compare-result nil))))
					   (list sequence1 sequence1-start sequence1-end-e)
					   (list test-sequence test-sequence-start test-sequence-end-e))
			 t)))
	(return-from cond-replace (values sequence1 nil)))
      (let* ((length-delta (- new-sequence-length test-sequence-length))
	     (adjust-by-fill-pointer? (and (not (zerop length-delta))
					   (vectorp sequence1)
					   (array-has-fill-pointer-p sequence1)
					   (<= 0
					       (+ (length sequence1) length-delta)
					       (cl:array-dimension sequence1 0)))))
	(cond ((zerop length-delta)
	       (values (replace sequence1 new-sequence
				:start1 sequence1-start :end1 sequence1-end-e
				:start2 new-sequence-start :end2 new-sequence-end-e)
		       t))
	      ((and (vectorp sequence1)
		    (or adjust-by-fill-pointer?
			(adjustable-array-p sequence1)))
	       ;; Adjust the vector.
	       (let ((sequence1-orig-length (length sequence1)))
		 (flet ((shift ()
			  (let ((source-start sequence1-end-e)
				(source-end sequence1-orig-length)
				(target-start (+ sequence1-end-e length-delta))
				(target-end (+ sequence1-orig-length length-delta)))
			    (assert (<= 0 source-start source-end sequence1-orig-length)
				    (source-start source-end sequence1-orig-length))
			    (assert (<= 0 target-start target-end (length sequence1))
				    (target-start target-end sequence1))
			    (assert (= (- target-end target-start)
				       (- source-end source-start))
				    (target-start target-end source-start source-end))
			    (replace sequence1 sequence1
				     :start1 target-start :end1 target-end
				     :start2 source-start :end2 source-end)))
			(adjust (new-size)
			  (cond (adjust-by-fill-pointer?
				 (setf (fill-pointer sequence1) new-size))
				(t (let ((new-array
					  (adjust-array sequence1 new-size
							:fill-pointer (coerce-boolean
								       (array-has-fill-pointer-p
									sequence1)))))
				     (assert (eq new-array sequence1)))))))
		   (when (minusp length-delta)
		     ;; If delta is positive, we haven't adjusted to
		     ;; be larger yet, so there's no room to shift
		     ;; right.  But we can (and must) shift now if
		     ;; it's negative.
		     (shift))
		   (adjust (+ sequence1-orig-length length-delta))
		   (when (plusp length-delta)
		     ;; If delta is negative, we've adjusted
		     ;; already, so we've lost the end of the
		     ;; original data.  But we can (and must) shift
		     ;; now if it's positive.
		     (shift))))
	       (values (replace sequence1 new-sequence
				:start1 sequence1-start :end1 (+ sequence1-end-e length-delta)
				:start2 new-sequence-start :end2 new-sequence-end-e)
		       t))
	      ((vectorp sequence1)
	       (let ((out (make-array (+ (length sequence1) length-delta)
				      :element-type (array-element-type sequence1)
				      :fill-pointer (coerce-boolean
						     (array-has-fill-pointer-p sequence1)))))
		 (replace out sequence1
			  :end1 sequence1-start
			  :end2 sequence1-start)
		 (replace out new-sequence
			  :start1 sequence1-start :end1 (+ sequence1-end-e length-delta)
			  :start2 new-sequence-start :end2 new-sequence-end-e)
		 (replace out sequence1
			  :start1 (+ sequence1-end-e length-delta)
			  :start2 sequence1-end-e)
		 (values out t)))
	      ((listp sequence1)
	       (multiple-value-bind (left-part remaining)
		   (split-list! sequence1-start sequence1)
		 (let ((middle-part (coerce (subseq new-sequence new-sequence-start
						    new-sequence-end-e)
					    'list))
		       (right-part (nthcdr sequence1-length remaining)))
		   (values (nconc left-part middle-part right-part)
			   t))))
	      (t (error "Unhandled case.")))))))

(defun split-list! (pos list)
  "Destructively splits LIST into two parts: the part before position
POS and the part after it.  Returns the two resulting parts.

POS must be at least 0 and no greater than the length of LIST."
  (declare (type (integer 0) pos))
  (if (zerop pos)
      (values '() list)
      (let* ((last-cell-of-first-list (nthcdr (1- pos) list))
	     (second-list (rest last-cell-of-first-list)))
	(setf (rest last-cell-of-first-list) '())
	(values list second-list))))

(defun subseq-displace (vector start &optional end)
  "Like subseq, but for vectors, returning a displaced vector which
shares structure with the given vector."
  (let ((end-effective (if (null end)
			   (length vector)
			   end)))
    (unless (<= 0 start end-effective (length vector))
      (error "The bounding indices ~S and ~S are bad for a sequence of length ~D."
	     start end (length vector)))
    (make-array (- end-effective start)
		;; :ELEMENT-TYPE is required for SBCL 0.9.16 to treat
		;; displaced strings as strings (see below).
		:element-type (array-element-type vector)
		:displaced-to vector :displaced-index-offset start)))
;;(let* ((s "foobar")
;;       (s-d1 (make-array 3 :displaced-to s :displaced-index-offset 2))
;;       (s-d2 (make-array 3 :element-type (array-element-type s)
;;			 :displaced-to s :displaced-index-offset 2)))
;;  (format t "~&(TYPE-OF ~S) => ~S"
;;	  s (type-of s))
;;  (let* ((table `(("" type-of array-element-type (typep x 'string) stringp)
;;		  (s-d1 ,(type-of s-d1) ,(array-element-type s-d1)
;;		   ,(typep s-d1 'string) ,(stringp s-d1))
;;		  (s-d2 ,(type-of s-d2) ,(array-element-type s-d2)
;;		   ,(typep s-d2 'string) ,(stringp s-d2))))
;;	 (elt-format "~A")
;;	 (table-f (map 'list (lambda (row)
;;			       (map 'list (lambda (elt)
;;					    (format nil elt-format elt))
;;				    row))
;;		       table))
;;	 (col-widths (map 'list (lambda (column)
;;				  (apply #'max (map 'list #'length column)))
;;			  (apply #'jpl-util:zip table-f))))
;;    (dolist (row table-f)
;;      (format t "~&  ~{~VA~^   ~}" (apply #'append
;;					  (apply #'jpl-util:zip (list col-widths row)))))))
;;
;;(TYPE-OF "foobar") => (SIMPLE-ARRAY CHARACTER (6))
;;         TYPE-OF                ARRAY-ELEMENT-TYPE   (TYPEP X 'STRING)   STRINGP
;;  S-D1   (VECTOR CHARACTER 3)   CHARACTER            NIL                 NIL    
;;  S-D2   (VECTOR CHARACTER 3)   CHARACTER            T                   T      

(defun subseq* (sequence start &optional end)
  "Like subseq, but shares structure with the input sequence (where
possible)."
  (etypecase sequence
    (vector
     (if (and (= start 0)
	      (or (null end)
		  (= end (length sequence))))
	 sequence
	 (subseq-displace sequence start end)))
    (list
     (let ((head (nthcdr start sequence))
	   (end (unless (null end) (- end start))))
       (if (or (null end)
	       (= (length sequence) end))
	   ;; Permitted to share cons cells.
	   head
	   (subseq head 0 end))))))


(defun any (sequence &key (key #'identity))
  "Returns the logical-or of all the generalized booleans in the given
sequence."
  (map nil (lambda (v)
	     (when (funcall key v)
	       (return-from any t)))
       sequence)
  nil)
(defun all (sequence &key (key #'identity))
  "Returns the logical-and of all the generalized booleans in the
given sequence."
  (map nil (lambda (v)
	     (unless (funcall key v)
	       (return-from all nil)))
       sequence)
  t)

(defun duplicates-p (sequence &key (test #'eql) (start 0) end (key #'identity))
  "Returns whether there are duplicates in the given sequence."
  (/= (length sequence)
      (length (remove-duplicates sequence :test test :start start :end end :key key))))

;;; SBCL 0.9.16.0 seems to exhibit worst-case time of O(n**2) for
;;; REMOVE-DUPLICATES.  Aside from the sequence-copy and sorting, this
;;; algorithm (below) exploits ordering to attain O(n) time.
;;; 
;;; CL-USER> (defparameter *random-numbers* (loop repeat 10000 collecting (random 10000.0)))
;;; *RANDOM-NUMBERS*
;;; CL-USER> (let* ((test-count 0)
;;;                 (test (lambda (n1 n2)
;;;                     (incf test-count)
;;;                     (= n1 n2))))
;;;            (let ((out (jpl-util:remove-ordered-duplicates *random-numbers* #'< :test test)))
;;;              (values (length out) test-count)))
;;; 9995
;;; 9999
;;; CL-USER> (let* ((test-count 0)
;;;              (test (lambda (n1 n2)
;;;                      (incf test-count)
;;;                      (= n1 n2))))
;;;            (let ((out (remove-duplicates *random-numbers* :test test)))
;;;              (values (length out) test-count)))
;;; 9995
;;; 49973123
;;;
;;; Sometimes REMOVE-DUPLICATES does really well:
;;; 
;;; CL-USER> (defparameter *random-numbers* (loop repeat 10000 collecting (random 100)))
;;; *RANDOM-NUMBERS*
;;; CL-USER> (let* ((test-count 0)
;;;                 (test (lambda (n1 n2)
;;;                         (incf test-count)
;;;                         (= n1 n2))))
;;;            (let ((out (jpl-util:remove-ordered-duplicates *random-numbers* #'< :test test)))
;;;              (values (length out) test-count)))
;;; 100
;;; 9999
;;; CL-USER> (let* ((test-count 0)
;;;                 (test (lambda (n1 n2)
;;;                         (incf test-count)
;;;                         (= n1 n2))))
;;;            (let ((out (remove-duplicates *random-numbers* :test test)))
;;;              (values (length out) test-count)))
;;; 100
;;; 989501
;;;
;;; REMOVE-DUPLICATES seems to win the more duplicates there are.
;;; CLHS doesn't specify what algorithm is to be employed.  Talk To
;;; Your Doctor, and measure your Lisp and your program before making
;;; a choice.
(defun remove-ordered-duplicates (sequence sort-predicate &key (test #'eql) (start 0) (end nil)
					   (key #'identity key-spec-p)
					   (sort-key #'identity sort-key-spec-p)
					   (comparison-key #'identity comparison-key-spec-p))
  (when key-spec-p
    (when (and sort-key-spec-p comparison-key-spec-p)
      (error ":KEY conflicts with :SORT-KEY and :COMPARISON-KEY arguments because it specifies \
both the sort and comparison keys."))
    (when sort-key-spec-p
      (error ":KEY conflicts with :SORT-KEY argument because it specifies both the sort and \
comparison keys.  Specify only the latter and :COMPARISON-KEY instead."))
    (when comparison-key-spec-p
      (error ":KEY conflicts with :COMPARISON-KEY argument because it specifies both the sort \
and comparison keys.  Specify only the latter and :SORT-KEY instead.")))
  (let ((sort-key (if key-spec-p key sort-key))
	(comparison-key (if key-spec-p key comparison-key)))
    (concatenate (typecase sequence
		   (vector
		    `(vector ,(array-element-type sequence)))
		   (list
		    'list)
		   (t (type-of sequence)))
		 (subseq* sequence 0 start)
		 (let ((seq-sorted (sort (subseq* sequence start end) sort-predicate :key sort-key)))
		   (reduce (lambda (item1 prior-items)
			     (if (endp prior-items)
				 (list item1)
				 (let ((item2 (first prior-items)))
				   (if (funcall test
						(funcall comparison-key item1)
						(funcall comparison-key item2))
				       (list* item1 (rest prior-items))
				       (list* item1 prior-items)))))
			   seq-sorted :initial-value '() :from-end t))
		 (if (null end)
		     '()
		     (subseq* sequence end)))))

(defun equivalent-hash-table-test (test)
  "Given a designator of a test function (one that returns a
generalized boolean indicating whether its two arguments are equal),
tries to find an alternative function that can be used as a hash-table
test, returning two values: a semantically-equivalent function of two
arguments, and a generalized boolean indicating whether the returned
function is a valid hash-table test function.

The returned function will behave the same as the given designated
function whenever it is given arguments which would be valid for the
original function.  If the returned function is given invalid
arguments, it might not signal an error in situations that the
original function would have."
  (macrolet ((function-case (key &rest clauses)
	       (with-gensyms (key%)
		 `(let ((,key% ,key))
		    (cond ,@(loop for (clause-keys . clause-body) in clauses
				  collecting
				  `((member ,key% ',clause-keys
					    :key #'symbol-function)
				    ,@clause-body)))))))
    (let* ((test-fn (designated-function test))
	   (equiv-fn (function-case test-fn
				    ((eq eql equal equalp) test-fn)
				    ((= char-equal string-equal) #'equalp)
				    ((string=) #'equal)
				    ((char=) #'eql))))
      (if (null equiv-fn)
	  (values test-fn nil)
	  (values equiv-fn t)))))

;;; FIXME: there must be more efficient ways to do this.  It seems
;;; like I'm also rewriting REMOVE-DUPLICATES.  Sigh.
(defun find-duplicates (sequence &key (test #'eql) (key #'identity))
  "Returns all duplicates in SEQUENCE, according to the TEST and KEY.
Each item that appears anywhere else in the sequence is counted.

If TEST is a standard Common Lisp function that only applies to
certain types (such as numbers, strings, or characters), a substitute
test may be used in order to enable fast linear-time duplicate search
with a hash-table.  In this case, if any element of SEQUENCE is not
appropriate for TEST, and TEST would normally signal an error, the
result of this function is undefined."
  (multiple-value-bind (ht-test ht-test-p)
      (equivalent-hash-table-test test)
    (if ht-test-p
	(find-duplicates-with-hash-table sequence ht-test key)
	(etypecase sequence
	  (list (find-duplicates-in-list sequence test key))
	  (vector (find-duplicates-in-vector sequence test key))))))
(defun find-duplicates-with-hash-table (sequence test key)
  (macrolet ((function-body (do-sequence with-accumulation element-type)
	       `(let ((count-by-keyed-elt (make-hash-table :test test))
		      keyed-elt
		      keyed-elt-occurrences
		      (dup-count 0)
		      (collected-dups 0))
		  (,do-sequence (elt sequence)
		    (setf keyed-elt (funcall key elt))
		    (setf keyed-elt-occurrences
			  (gethash keyed-elt count-by-keyed-elt 0))
		    (incf dup-count (cond ((= keyed-elt-occurrences 1) 2)
					  ((> keyed-elt-occurrences 1) 1)
					  (t 0)))
		    (setf (gethash keyed-elt count-by-keyed-elt)
			  (1+ keyed-elt-occurrences)))
		  (,with-accumulation (set-size/type acc)
		    (set-size/type dup-count ,element-type)
		    (,do-sequence (elt sequence)
		      (when (> (gethash (funcall key elt) count-by-keyed-elt) 1)
			(incf collected-dups)
			(assert (<= collected-dups dup-count)
				() "More duplicates found on the second ~
                                    pass (~D+) than were found on first ~
                                    pass (~:*~D).  The sequence is changing, ~
                                    or KEY is returning non-equivalent results."
				dup-count)
			(acc elt))))))
	     (do-vector ((elt-var vector) &body body)
	       `(loop for ,elt-var across ,vector
		      doing (progn ,@body)))
	     (accumulate-to-list/size/type ((set-size/element-type accumulate)
					    &body body)
	       `(macrolet ((,set-size/element-type (size &optional type)
			     (declare (ignore size type))))
		  (accumulate-to-list (,accumulate) ,@body))))
    (etypecase sequence
      (list (function-body dolist accumulate-to-list/size/type
			   't))
      (vector (function-body do-vector accumulate-to-vector
			     (array-element-type sequence))))))
(defun find-duplicates-in-list (list test key)
  (declare (type list list))
  (loop for (item . rest-items) on list
	for item-key = (funcall key item)
	when (or
	      ;; Known prior dup?
	      (member item-key dups :test test :key key)
	      ;; Equals future item?
	      (member item-key rest-items :test test :key key))
	collect item into dups
	finally (return dups)))
(defun find-duplicates-in-vector (vector test key)
  (declare (type vector vector))
  (loop for i from 0 below (length vector)
	for item = (aref vector i)
	for item-key = (funcall key item)
	when (or
	      ;; Known prior dup?
	      (member item-key dups :test test :key key)
	      ;; Equals future item?
	      (not (null (position item-key vector :start (1+ i)
				   :test test :key key))))
	collect item into dups and count 1 into dup-count
	;; Use MAKE-ARRAY instead of (COERCE .. 'SIMPLE-VECTOR) to get
	;; a nice specialized array type.
	finally (return (make-array dup-count
				    :element-type (array-element-type vector)
				    :initial-contents dups))))

;;; FIXME: I'm certain this exists somewhere (maybe even in CL), but I
;;; can't find it.
;;; 
;;; FIXME: define a MAP-UNIQUE-PAIRS function to cons less in the
;;; seemingly typical case that we just want to iterate over all
;;; unique pairs.
;;; 
;;; FIXME: This function would be better named ORDERED-PAIRS.
;;; "UNIQUE-PAIRS" is misleading; it implies that duplicate pairs
;;; (ordered or even unordered) are removed.
(defun unique-pairs (list &optional predicate)
  "Returns the set of each possible ordered pair of elements in LIST.
Each ordered pair is represented as a list.  The returned set is
represented as a list.

When PREDICATE is given and not NIL, it must be function.  PREDICATE
is applied to each pair of elements to filter them from the result
set; pairs for which PREDICATE returns false are omitted."
  (loop for (elt1 . rest) on list nconcing
	(loop for elt2 in rest
	      when (or (not predicate)
		       (funcall predicate elt1 elt2))
	      collecting (list elt1 elt2))))

(defun adjacent-pairs (sequence)
  "For each two adjacent pairs of elements in the given SEQUENCE,
return them as a list.

If SEQUENCE has less than 2 elements, returns the empty list.
Otherwise, returns (1- (LENGTH SEQUENCE)) pairs."
  (map-adjacent-pairs 'list #'list sequence))

(defun map-adjacent-pairs (result-type function sequence)
  "Applies FUNCTION to every adjacent pair of elements in SEQUENCE (in
sequence).  Returns NIL if RESULT-TYPE is NIL, otherwise returns a
sequence of the values obtained by each call of FUNCTION.  The type of
SEQUENCE is as in MAP.

If SEQUENCE has less than 2 elements, FUNCTION is never called.
Otherwise, it is called (1- (LENGTH SEQUENCE)) times.

The consequence of FUNCTION modifying SEQUENCE is undefined."
  (declare (type function-designator function)
	   (type sequence sequence))
  ;; FIXME: This is the price for flexibility and cons-avoidance, and
  ;; it's is ugly.  We should put most of this code in a new function
  ;; or macro for user-defined MAP-like functions.
  (macrolet ((iterate-adjacent-pairs (exhausted? next accumulate)
	       `(loop for first-p = t then nil
		      for last-element = nil then element
		      for end-p = ,exhausted?
		      for element = (unless end-p ,next)
		      until end-p
		      unless first-p
		      doing (,accumulate
			     (funcall function last-element element)))))
    (with-sequence-iterator (next size exhausted? sequence)
      (cond ((null result-type)
	     (iterate-adjacent-pairs (exhausted?) (next) progn))
	    ((subtypep result-type 'list)
	     (accumulate-to-list (acc)
	       (iterate-adjacent-pairs (exhausted?) (next) acc)))
	    ((subtypep result-type 'vector)
	     (let ((result-length (max 0 (1- (size)))))
	       (multiple-value-bind (element-type requested-size)
		   (parse-vector-type result-type)
		 (unless (or (eq requested-size '*)
			     (= requested-size result-length))
		   (error "The RESULT-TYPE ~S specifies a size of ~D, but ~
                           ~D element~:P are required to store the result."
			  result-type requested-size result-length))
		 (accumulate-to-vector (set-size/type acc)
		   (set-size/type result-length element-type)
		   (iterate-adjacent-pairs (exhausted?) (next) acc)))))
	    (t (error "The result-type ~S is not NIL or a ~
                       recognizable subtype of LIST or VECTOR."
		      result-type))))))

(defun parse-vector-type (type)
  "Given a type specifier which is a recognizable subtype of VECTOR,
returns the element type and the sequence length (or * for unbounded
length).

If either of these parameters cannot be determined, the least
restricting value is used: T for element type, and * for length."
  (unless (subtypep type 'vector)
    (error "~S does not designate a recognizable subtype of VECTOR." type))
  (destructuring-bind (class-name &rest args)
      (if (atom type)
	  `(,type)
	  type)
    (labels ((parse-type/size ()
	       (destructuring-bind (&optional (type 't) (size '*)) args
		 (ensure-valid-size size)
		 (values type size)))
	     (parse-size (type)
	       (destructuring-bind (&optional (size '*)) args
		 (ensure-valid-size size)
		 (values type size)))
	     (ensure-valid-size (size)
	       (unless (or (eq size '*)
			   (typep size 'array-dimension))
		 (error "Size ~S in type specifier ~S is not a ~
                         valid array dimension (or *)."
			size type))))
      (case class-name
	(vector (parse-type/size))
	(simple-vector (parse-size 't))
	((bit-vector simple-bit-vector) (parse-size 'bit))
	((string simple-string) (parse-size 'character))
	((base-string simple-base-string) (parse-size 'base-char))
	(otherwise (values 't '*))))))

(defun parse-sequence-type (type)
  "Given a type specifier which is a subtype of SEQUENCE, returns the
primary sequence type (LIST or VECTOR), the element type, and the
sequence length (or * for unbounded length).

If either of the latter two parameters cannot be determined, the least
restricting value is used: T for element type, and * for length."
  (unless (subtypep type 'sequence)
    (error "~S does not designate a recognizable subtype of SEQUENCE." type))
  (assert (subtypep type '(or list vector)))
  (if (subtypep type 'list)
      (values 'list 't '*)
      (multiple-value-bind (element-type length) (parse-vector-type type)
	(values 'vector element-type length))))

(defmacro list-extract! (list predicate &key mapping-p)
  "Returns those items in list that pass the predicate, removing them
destructively from the input.  When mapping-p is true, returns the
result of the predicate function for each element, rather than the
elements themselves."
  (let ((results-var (gensym "RESULTS-"))
	(new-list-var (gensym "NEW-LIST-")))
    `(multiple-value-bind (,results-var ,new-list-var)
         (list-extract!-helper ,list ,predicate ,mapping-p)
       (setf ,list ,new-list-var)
       ,results-var)))

(defun list-extract!-helper (list predicate mapping-p)
  (let* ((modified-list (list* nil list)) ; Add guard value.
	 (out-list (loop for prior-cell = nil then cell
			 for cell on modified-list
			 for item = (first cell)
			 for mapped-value = (unless (null prior-cell)
					      (funcall predicate item))
			 when mapped-value
			 ;; FIXME: Should probably reuse this cell,
			 ;; while we're being destructive...
			 collecting (progn
				      (setf (rest prior-cell)
					    (rest cell))
				      (setf cell prior-cell)
				      (if mapping-p
					  mapped-value
					  item)))))
    (values out-list (rest modified-list))))

(defun delete-nth! (n list)
  (cond ((zerop n)
	 (when (endp list)
	   (error "Index ~D exceeds length of list." n))
	 (rest list))
	(t
	 (let* ((prior-cell (nthcdr (1- n) list))
		(to-delete-cell (rest prior-cell))
		(following-cell (rest to-delete-cell)))
	   (when (or (endp prior-cell)
		     (endp to-delete-cell))
	     (error "Index ~D exceeds length of list." n))
	   (setf (cdr prior-cell)
		 following-cell)
	   list))))

(defun insert-at (list n item)
  "Returns a new list with the given ITEM inserted into the LIST at position N."
  (cond ((zerop n)
	 (list* item list))
	(t
	 (append (subseq list 0 n)
		 (list item)
		 (nthcdr n list)))))

(defun best (set predicate &key (key #'identity) allow-empty)
  "Finds the best element in the given SET of elements (a SEQUENCE).
The given PREDICATE function of two arguments must return whether the
first \"applicable value\" represents a better item than the item
represented by the second applicable value.

Elements are their own applicable value unless KEY is supplied.

When ALLOW-EMPTY, SET may be empty, in which case NIL is returned.
Otherwise, an error is signalled.

This is more efficient than sorting and taking the first item from the
result, and it is more general than MIN and MAX."
  (declare (type sequence set)
	   (type function-designator predicate key))
  (cond ((empty? set)
	 (unless allow-empty
	   (error "SET is empty, and ALLOW-EMPTY is false.")))
	(t (let* ((best-item (elt set 0))
		  (best-value (funcall key best-item)))
	     (map nil (lambda (item)
			(let ((value (funcall key item)))
			  (when (funcall predicate value best-value)
			    (setf best-item item
				  best-value value))))
		  set)
	     best-item))))


;;; FIXME: This stuff sucks and needs to go.
(defclass expr-trace ()
  ((id :type integer :initarg :id :initform (error "Must supply id.")
       :reader id)
   (expression :initarg :expression :initform (error "Must supply expression.")
	       :accessor expression)
   (home-package :type package :initarg :home-package
		 :initform (error "Must supply home-package") :reader home-package)))

(defun expr-trace-expression-format (expr-trace)
  (if (and (listp (expression expr-trace))
	   (not (endp (expression expr-trace))))
      "~{~S~^ ~}"
      "~S"))

(defvar *expr-traces-by-id*)
(defvar *expr-traces-by-expr*)

(defun clear-traces! ()
  "Because we can't tell whether code being traced with WITH-TRACE is
still active in the system, or it's been redefined to something else
with a new trace-ID, *expr-traces-by-id* always grows.  It grows only
when WITH-TRACE is macroexpanded, so you should not normally need this
function."
  (setf *expr-traces-by-id* (make-hash-table :test #'equalp))
  (setf *expr-traces-by-expr* (make-hash-table :test #'equalp))
  (values))

(defun list-traces (&key (stream t stream-spec-p) packages)
  (unless stream-spec-p
    (setf stream *standard-output*))
  (when (null packages)
    (setf packages (list *package*)))
  (unless (eq packages t)
    (unless (listp packages)
      (setf packages (list packages)))
    (setf packages (map 'list #'find-package packages)))
  (let* ((traces (loop for expr-trace being each hash-value in *expr-traces-by-id*
		       collecting expr-trace))
	 (traces-filtered (if (eq packages t) traces
			      (remove-if (lambda (trace)
					   (not (member (home-package trace) packages
							:test #'eq)))
					 traces)))
	 (traces-sorted (nsort traces-filtered
			       (lambda (id1 id2)
				 (cond ((and (numberp id1) (numberp id2))
					(< id1 id2))
				       ((and (symbolp id1) (symbolp id2))
					(string< id1 id2))
				       ((and (numberp id1) (symbolp id2))
					t)
				       ((and (symbolp id1) (numberp id2))
					nil)))
			       :key #'id)))
    (dolist (trace traces-sorted)
      (format stream "~&")
      (let ((orig-package *package*)
	    (*package* (home-package trace)))
	(when (not (member orig-package (list* *package* (package-used-by-list *package*))
			   :test #'eq))
	  (let ((package-nickname (best (list* (package-name *package*)
					       (package-nicknames *package*))
					#'< :key #'length)))
	    (format stream "~A:" package-nickname)))
	(let ((expr-format (expr-trace-expression-format trace)))
	  (format stream "~S: ~?~%" (id trace) expr-format (list (expression trace))))))))

(eval-when (:load-toplevel :execute)
  (clear-traces!))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((counter 0))
    (defun register-trace (expression &optional id)
      (let (trace)
	(unless (null id)
	  (setf trace (gethash id *expr-traces-by-id*))
	  (setf (expression trace) expression))
	(when (null trace)
	  (setf trace (gethash expression *expr-traces-by-expr*)))
	(when (null trace)
	  (when (null id)
	    (setf id (incf counter)))
	  (setf trace (make-instance 'expr-trace :id id
				     :expression expression
				     :home-package *package*))
	  (setf (gethash (id trace) *expr-traces-by-id*) trace))
	(setf (gethash expression *expr-traces-by-expr*) trace)
	trace))))
      
(defmacro with-trace ((&optional id) &body body)
  "Like TRACE, but for the given body of code.  Specify an ID (a
number or symbol) to avoid the conceptually-same code from clogging up
the output of LIST-TRACES as this code gets redefined several times."
  (let* ((trace (register-trace body id))
	 (*package* (home-package trace))
	 (label-full (format nil (expr-trace-expression-format trace)
			     (list (expression trace))))
	 (label (if (<= (length label-full) 30)
		    label-full
		    (format nil "~S" (id trace)))))
    (let ((out% (gensym)))
      `(progn
	(format *trace-output* "~&Enter: ~A~%" ,label)
	(let ((,out% (multiple-value-list (progn ,@body))))
	  (format *trace-output* "~&Exit: ~A:~{~&  ~S~}~%" ,label ,out%)
	  (values-list ,out%))))))

(defun get-reasonable-real-time ()
  "Returns the current real time in seconds, possibly with sub-second
precision.  Not guaranteed to be CL universal time."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun get-reasonable-run-time ()
  "Returns the current CPU time in seconds, possibly with sub-second
precision.  Not guaranteed to be CL universal time."
  (/ (get-internal-run-time) internal-time-units-per-second))

;;; Copied from Paul Khuong; implements Fisher-Yates.
(defun shuffle! (sequence)
  "Shuffles the given sequence in-place."
  (loop with vector = (coerce sequence 'vector)
	for i downfrom (1- (length vector)) to 0
	doing (rotatef (aref vector i)
		       (aref vector (random (1+ i))))
	finally (return (replace sequence vector))))
(defun shuffle (sequence)
  (let ((sequence-copy
	 (make-array (length sequence)
		     :element-type (if (typep sequence 'vector)
				       (array-element-type sequence)
				       't)
		     :initial-contents sequence)))
    (shuffle! sequence-copy)
    (coerce sequence-copy (type-of sequence))))

;;; Copied from Hunchentoot.
(defmacro defvar-unbound (name &optional doc-string)
  "Convenience macro to declare unbound special variables with a
documentation string."
  (check-type name symbol)
  `(progn
    (defvar ,name)
    ,@(if (null doc-string) '()
	  `((setf (documentation ',name 'variable) ,doc-string)))
    ',name))

(defun integer->twos-complement (n &optional size-multiple)
  "Returns the representation of N in two's complement, as a
bit-array.  If SIZE-MULTIPLE is given, the length of the result will
be a multiple of SIZE-MULTIPLE."
  (check-type n integer)
  (check-type size-multiple (or null (integer 1)))
  (let* ((n-length (integer-length n))
	 (bits-length (1+ n-length))
	 (bits-length (if (null size-multiple)
			  bits-length
			  (* (ceiling bits-length size-multiple)
			     size-multiple)))
	 (bits (make-array bits-length :element-type 'bit))
	 (bits-ub (1- bits-length))
	 (x (if (minusp n)
		(1- (abs n))
		n)))
    (dotimes (i n-length)
      (setf (bit bits (- bits-ub i)) (mod x 2))
      (setf x (ash x -1)))
    (when (minusp n)
      (bit-not bits t))
    bits))

(defun twos-complement->integer (bit-vector)
  "Returns the integer represented in two's complement within
BIT-VECTOR."
  (loop for bit across bit-vector
	for i from 0
	for place-pos from (1- (length bit-vector)) downto 0
	unless (zerop bit)
	summing (* (expt 2 place-pos)
		   (if (zerop i) -1 1))))

(defvar *verbose* nil
  "Whether to be verbose when evaluating forms expressed in VERBOSELY.")
(defvar *verbosity-depth* 0
  "The number of containing WITH-VERBOSITY forms that specify
verbosity to be enabled.")
(defmacro verbosely ((control-string &rest format-arguments) &body body)
  `(verbosely% (lambda () ,@body)
    ,control-string ,@format-arguments))
(defun verbosely% (function control-string &rest format-arguments)
  (when *verbose*
    (format *trace-output* "~&~V@T~?"
	    (* 2 (1- *verbosity-depth*))
	    control-string format-arguments))
  (funcall function))
(defmacro with-verbosity ((&optional (verbose nil verbose-p)) &body body)
  `(with-verbosity% (lambda () ,@body)
    ,@(when verbose-p `(,verbose))))
(defun with-verbosity% (function &optional (verbose *verbose*))
  (let ((*verbose* verbose)
	(*verbosity-depth* (if verbose
			       (1+ *verbosity-depth*)
			       *verbosity-depth*)))
    (funcall function)))


;; Yet another bit that seemed like a good idea but ends up unneeded.
(defmacro handler-case/no-unwind (form &rest clauses)
  "Like HANDLER-CASE, but does not unwind the stack before executing
an error-handling clause."
  ;; FIXME: I stole the general design here from the Notes part of the
  ;; CLHS page for HANDLER-CASE, which states the example code as
  ;; "approximately equivalent to."  Can we be more confident in
  ;; correctness here?
  (labels ((no-error-clause? (clause)
	     (eq :no-error (first clause)))
	   (handler-bind-form (error-clauses exit-block)
	     `(handler-bind
		  ,(map 'list (curry-right #'handler-bind-clause exit-block)
			error-clauses)
		,form))
	   (handler-bind-clause (error-clause exit-block)
	     (destructuring-bind
		   (typespec (&optional (var (gensym "CONDITION-") var-p))
			     &body body)
		 error-clause
	       `(,typespec (lambda (,var)
			     ,@(unless var-p `((declare (ignore ,var))))
			     (return-from ,exit-block
			       (locally ,@body)))))))
    (unless (<= (count-if #'no-error-clause? clauses) 1)
      (error "Too many :NO-ERROR clauses."))
    (let ((no-error-clause (find-if #'no-error-clause? clauses))
	  (error-clauses (remove-if (complement #'no-error-clause?) clauses))
	  (exit% (gensym "EXIT-")))
      `(block ,exit%
	 ,(if (null no-error-clause)
	      (handler-bind-form error-clauses exit%)
	      (destructuring-bind (lambda-list &body body)
		  (rest no-error-clause)
		`(multiple-value-call (lambda ,lambda-list ,@body)
		   ,(handler-bind-form error-clauses exit%))))))))

;;; Copied from CL-GD.
(defmacro with-safe-alloc ((var alloc free) &rest body)
  "Binds VAR to NIL, then sets it to the result of evaluating ALLOC.
Upon exit of dynamic extent, as long as the value of VAR is not NIL
\(such as by a condition signalled by evaluating ALLOC), FREE is
evaluated."
  `(let ((,var nil))
     (unwind-protect
         (progn (setf ,var ,alloc)
                ,@body)
       (unless (null ,var) ,free))))


(defun format-ordinal (n)
  "Formats n to a short-hand ordinal: 1st, 2nd, etc."
  (format nil "~D~A"
	  n (cond ((< 10 (mod (abs n) 100) 20) "th")
		  ((= 1 (mod (abs n) 10)) "st")
		  ((= 2 (mod (abs n) 10)) "nd")
		  ((= 3 (mod (abs n) 10)) "rd")
		  (t "th"))))

(defun a/an-number (n)
  "Given a non-negative integer that will be expressed as a cardinal
or ordinal number in English--either by FORMAT-ORDINAL or the ~R or
~:R format directives--returns either \"a\" or \"an\", whichever is
appropriate to precede the number."
  (declare (type (integer 0) n))
  ;; The way ordinals and cardinals are written in long-hand and read
  ;; aloud is to break the number into groups of 3 decimal digits
  ;; (from the right), then read each group along with the magnitude
  ;; of that group.  So, 89,274 is "eighty-nine thousand [next group:]
  ;; two-hundred seventy four."
  ;; 
  ;; From experimentation, but not by any kind of formal analysis of
  ;; English, "an" is indicated whenever the first group:
  ;;   
  ;;   * Is 11: "an eleven digit number", "an eleven thousand two
  ;;     hundred seventy-eight byte file"
  ;;   
  ;;   * Is 18: "an 18-wheeler"
  ;;   
  ;;   * Starts with the digit 8: "eight", "eighty", "eight hundred",
  ;;     "eight thousand", "eighty-nine thousand one hundred sixteen",
  ;;     "eight hundred seventy-nine billion ...", ...
  ;; 
  ;; In all other cases, "a" is called for.
  (let ((first-group (first (integer-digits n 1000))))
    (if (or (= 11 first-group)
	    (= 18 first-group)
	    (let ((first-group-digits (integer-digits first-group 10)))
	      (= 8 (first first-group-digits))))
	"an" "a")))

;; FORMAT deep-magic from CLHS 22.3.7.2: Tilde Left-Bracket:
;; Conditional Expression.
(defun english-list-format-control (&optional (element-format "~A") (relation "and"))
  "Creates a format control to format its operands as a list in
English.  Expects at least one operand.

Each operand will be formatted with ELEMENT-FORMAT, a format control
that consumes one argument.  ELEMENT-FORMAT defaults to \"~A\".

RELATION is a string which describes the relation between each
element, such as \"and\" (the default for RELATION) or \"or.\""
  (declare (type string element-format relation))
  (format nil "~~#[~~;~A~~;~A ~A ~A~~:;~~@{~~#[~~;~A ~~]~A~~^, ~~}~~]"
         element-format element-format relation element-format relation
	 element-format))

(defun fractional (input denominators)
  "Finds the closest approximation of INPUT of the form (+ I (/ N D)),
where I is an integer with the same sign as INPUT, N is a numerator of
the same sign as INPUT, and D is a denominator from the non-empty list
of positive integers DENOMINATORS.  Returns the values I, N, and D.

For example, with DENOMINATORS equal to (2 3 4), here are
some results from example values of INPUT:

  0.5 => 0 1 2
  3.667 => 3 2 3
  365.2425 => 365 1 4

It is conventional to express U.S. customary units with fractions,
rather than decimal numbers.  You can use FRACTIONAL to do this:

  (defun format-inches (stream inches)
    (multiple-value-bind (i n d)
	(jpl-util:fractional inches '(2 4 8 16))
      ;; If you're really clever, I'm sure you can squeeze this into
      ;; one big FORMAT control.
      
      ;; Explicitly print minus sign, rather than use the sign of i,
      ;; in-case i is zero but n is negative.
      (when (minusp inches)
	(princ #\- stream))
      (princ (abs i) stream)
      (unless (zerop n)
	(format stream \" ~D/~D\" (abs n) d))
      (princ \" in\" stream))
    (values))

  (format-inches t -0.07)  => -0 1/16 in
  (format-inches t 3.882)  => 3 7/8 in
  (format-inches t 11.997) => 12 in"
  (declare (optimize safety debug)
	   (type real input))
  (labels ((fraction (d)
	     (multiple-value-bind (i frac-part)
		 (truncate input)
	       (let* ((n (round (* frac-part d)))
		      (approximation (fraction-value i n d)))
		 ;; Update I, N incase we chose N = D.
		 (multiple-value-bind (i frac-part)
		     (truncate approximation)
		   (let ((n (* frac-part d)))
		     (list i n d))))))
	   (fraction-value (i n d)
	     (+ i (/ n d)))
	   (fraction-abs-error (fraction)
	     (abs (- input (apply #'fraction-value fraction)))))
    (let ((approximations (if (zerop (mod input 1))
			      ;; Checking for this case protects
			      ;; against division-by-zero, and makes
			      ;; sense anyway.
			      `((,input 0 ,(first denominators)))
			      (map 'list #'fraction denominators))))
      (values-list
       (best approximations #'< :key #'fraction-abs-error)))))

(defun ensure-type (value typespec &optional string)
  "A functional variant of CHECK-TYPE.  When VALUE is of the type
specified by TYPESPEC, returns VALUE.  Otherwise, signals a
correctable error of type TYPE-ERROR.  In this case, this function
will return only if the STORE-VALUE restart is invoked with a new
value that matches TYPESPEC; the new value will be returned.  If
STORE-VALUE is invoked with a new value that doesn't match TYPESPEC,
the process continues by raising another TYPE-ERROR.

As with CHECK-TYPE, \"STRING should be an English description of the
type, starting with an indefinite article ('a' or 'an').\""
  (loop with new-value = value
	until (typep new-value typespec)
	doing (setf new-value
		    (restart-case (error 'simple-type-error
					 :format-control "The given value is ~S, which is not ~:[of type ~S~;~:*~A~]."
					 :format-arguments `(,new-value ,string ,typespec)
					 :datum new-value :expected-type typespec)
		      (store-value (x)
			:report "Supply a new value."
			:interactive read-new-value
			x)))
	finally (return new-value)))

(defmacro check-type* (expression typespec &optional string)
  "Like CHECK-TYPE, with two differences: this tests any expression,
not just a place.  If an error is signalled, it won't be correctable."
  (with-gensyms (value%)
    `(let ((,value% ,expression))
       (unless (typep ,value% ',typespec)
	 (error 'simple-type-error
		:format-control "The given value is ~S, which is not ~
                                 ~:[of type ~S~;~:*~A~]."
		:format-arguments (list ,value% ,string ',typespec)
		:datum ,value% :expected-type ',typespec)))))

(defun read-new-value ()
  "Prompts the user on *QUERY-IO* for a value.  The value is expressed
by the user as a single form, which is evaluated.  The first resulting
value, or NIL if there was no value, is returned as a list of one
element.

Intended for :INTERACTIVE options to RESTART-CASE clauses."
  (format *query-io* "~&Enter a form to be evaluated:~&")
  (list (eval (read *query-io*))))

(defun standard-deviation (sequence)
  "Returns the standard deviation of SEQUENCE.  Each element of
SEQUENCE must be a number."
  (declare (type sequence sequence))
  (when (zerop (length sequence))
    (error "SEQUENCE must not be empty."))
  (sqrt (- (/ (reduce #'+ sequence
		      :initial-value 0 :key #'square)
	      (length sequence))
	   (square (mean sequence)))))

(defun mean (sequence)
  "Returns the arithmetic mean of SEQUENCE.  Each element of SEQUENCE
must be a number."
  (declare (type sequence sequence))
  (when (zerop (length sequence))
    (error "SEQUENCE must not be empty."))
  (/ (reduce #'+ sequence :initial-value 0)
     (length sequence)))

(defun square (x)
  (declare (type number x))
  (* x x))

(defun empty? (seq)
  "Returns a generalized boolean indicating whether SEQ (a SEQUENCE)
is empty."
  (declare (type sequence seq))
  (typecase seq
    (list (endp seq))
    (otherwise (zerop (length seq)))))

(defun circular-list (list)
  "Returns a fresh circular list that cycles through the contents of
LIST indefinitely.  If LIST is empty, the result will be empty (and
hence not actually circular).  LIST must be a proper list; if LIST is
circular, this function may not return." ; "May not return" to leave
					 ; room for more robust
					 ; behavior later.
  (declare (type list list))
  (let ((out (copy-list list)))
    (unless (endp out)
      (let ((last-cell (last out)))
	(unless (null (cdr last-cell))
	  (error "LIST is not a proper list."))
	(setf (rest last-cell) out)))
    out))

(defmacro with-extent-hooks ((enter return non-local-exit) &body body)
  "Evaluates BODY, and evaluates the given forms upon certain
situations relating to the dynamic extent of the evaluation of BODY.

ENTER must be a form.  Just before dynamic extent enters BODY, ENTER
is evaluated.  The return values of ENTER are discarded.

RETURN must be a list of the form (LAMBDA-LIST &BODY RETURN-BODY), or
NIL.  If BODY returns and RETURN is not NIL, RETURN-BODY (an implicit
progn) is evaluated with the variables of LAMBDA-LIST (an ordinary
lambda list) bound to the return values of BODY.  The return values of
RETURN-BODY are discarded.  The return values of BODY are returned by
WITH-EXTENT-HOOKS.

NON-LOCAL-EXIT must be a form.  If dynamic extent leaves BODY due to a
non-local exit (including due to a signalled condition),
NON-LOCAL-EXIT is evaluated.  The return values of NON-LOCAL-EXIT are
discarded."
  (let ((enter-fn `(lambda () ,enter))
	(return-fn (destructuring-bind (lambda-list &body return-body) return
		     `(lambda ,lambda-list ,@return-body)))
	(non-local-exit-fn `(lambda () ,non-local-exit))
	(main-fn `(lambda () ,@body)))
    `(with-extent-hooks% ,enter-fn ,return-fn ,non-local-exit-fn ,main-fn)))

(defun with-extent-hooks% (enter-fn return-fn non-local-exit-fn main-fn)
  "Function-based variant of WITH-EXTENT-HOOKS; see its documentation.

ENTER-FN must be a function of no arguments.  RETURN-FN must be a
function that can accept one argument for each value returned by
MAIN-FN.  NON-LOCAL-EXIT-FN must be a function of no arguments.
MAIN-FN must be a function of no arguments."
  (funcall enter-fn)
  (let ((success? nil))
    (unwind-protect
	 (let ((values (multiple-value-list (funcall main-fn))))
	   (setf success? t)
	   (apply return-fn values)
	   (values-list values))
      (unless success?
	(funcall non-local-exit-fn)))))

(defun decode-time-duration (duration &key
			     subsecond-digits (rounding 'truncate))
  "Breaks down a duration of time (as a non-negative REAL number of seconds),
returning days, hours, minutes, seconds, and subseconds.

The days, hours, and minutes values will be integers, while the
seconds value may be fractional (the same type as DURATION) if
DURATION is fractional and SUBSECOND-DIGITS is NIL; otherwise, the
seconds value will be an integer.

When SUBSECOND-DIGITS is NIL, the subseconds value will be NIL and the
seconds value will be of the same type as DURATION.  Otherwise,
SUBSECOND-DIGITS must be a non-negative integer that indicates to how
many decimal places subsecond precision will be kept, and the
subseconds value will be the true amount of subseconds (in [0,1),
rounded according to ROUNDING and SUBSECOND-DIGITS, and expressed as a
non-negative integer below (EXPT 10 SUBSECOND-DIGITS).

ROUNDING is a symbol that controls how subseconds values are rounded,
and may be FLOOR, CEILING, ROUND, or TRUNCATE.

Days in which no leap seconds occur are assumed.  (That is, the
returned number of days assumes 24 hours in one day, which does not
hold true on leap seconds days.)"
  (declare (type (real 0) duration)
	   (type (or (integer 0) null) subsecond-digits)
	   #+acl (type (member floor ceiling round truncate) rounding))
  ;; Handle subseconds up front.  Otherwise, CEILING could push the
  ;; number of seconds-of-minute up to 60, which needs to carry over
  ;; to minutes-of-hour and soforth.
  (multiple-value-bind (duration subsecond)
      (if (null subsecond-digits)
	  (values duration nil)
	  (let* ((subsecond-max (expt 10 subsecond-digits))
		 ;; This nicely normalizes things in the case of CEILING.
		 (subseconds-total (funcall rounding duration
					    (/ subsecond-max))))
	    (truncate subseconds-total subsecond-max)))
    (multiple-value-bind (minute second) (floor duration 60)
      (multiple-value-bind (hour minute) (floor minute 60)
	(multiple-value-bind (day hour) (floor hour 24)
	  (values day hour minute second subsecond))))))

(defun format-time-duration (seconds &key
			     (subsecond-digits 0)
			     (rounding 'truncate))
  "Breaks down the given number of seconds into day, hour, minute, and
seconds components, formatting it as a string.  The number of seconds
may be negative.

FRAC-DIGITS is the number of digits to show after the decimal point of
the seconds component (with the decimal point elided if 0).  ROUNDING
is a symbol that controls how subsecond precision is handled: it may
be either FLOOR, CEILING, ROUND, or TRUNCATE."
  (declare (type real seconds)
	   (type (integer 0) subsecond-digits)
	   #+acl (type (member floor ceiling round truncate) rounding))
  (let* ((orig-seconds seconds)
	 (subsecond-precision (expt 1/10 subsecond-digits))
	 ;; Since we give DECODE-TIME-DURATION the absolute value, the
	 ;; semantics of FLOOR and CEILING would be screwed up if we
	 ;; didn't pre-round for a negative number.
	 (rounded-seconds (* (funcall rounding seconds subsecond-precision)
			     subsecond-precision)))
    (multiple-value-bind (days hours minutes seconds subseconds)
	(decode-time-duration (abs rounded-seconds)
			      :subsecond-digits subsecond-digits)
      (when (zerop days)
	(setf days nil)
	(when (zerop hours)
	  (setf hours nil)
	  (when (zerop minutes)
	    (setf minutes nil))))
      (format nil "~:[~;-~]~@[~3Dd ~]~@[~2Dh ~]~@[~2Dm ~]~2D~[~*~:;~:*.~V,'0D~]s"
	      (minusp orig-seconds) days hours minutes seconds
	      subsecond-digits subseconds))))

(defun integer-digits (int &optional (radix 10))
  "Expresses the given non-negative integer as a fresh list of digits
in the given radix.  The list of digits is ordered most-significant
first.  When INT is 0, a list with just 0 is returned."
  (declare (type (integer 0) int)
	   (type (integer 2) radix)
	   (optimize speed))
  (let ((digits '()))
    (cond ((zerop int)
	   (push 0 digits))
	  (t (loop with n = int
		   until (zerop n)
		   doing (multiple-value-bind (rest-n digit)
			     (truncate n radix)
			   (push digit digits)
			   (setf n rest-n)))))
    digits))

(defun integer-digit-count (n &optional (radix 10))
  "Returns the number of digits required to represent the given
non-negative integer in RADIX.

  (integer-digit-count N RADIX) == (length (integer-digits N RADIX))"
  (declare (type (integer 0) n)
	   (type (integer 2) radix)
	   (optimize speed))
  (labels ((try-estimate (digits)
	     (let* ((lb (expt radix (1- digits)))
		    (ub (* radix lb)))
	       (cond ((< n lb)
		      (try-next-lower digits lb))
		     ((>= n ub)
		      (try-next-higher digits ub))
		     (t digits))))
	   (try-estimate/descending (digits lb)
	     (if (< n lb)
		 (try-next-lower digits lb)
		 digits))
	   (try-next-lower (digits lb)
	     (try-estimate/descending (1- digits) (truncate lb radix)))
	   (try-estimate/ascending (digits ub)
	     (if (>= n ub)
		 (try-next-higher digits ub)
		 digits))
	   (try-next-higher (digits ub)
	     (try-estimate/ascending (1+ digits) (* ub radix))))
    (if (zerop n)
	1
	(try-estimate (if (< n #.(expt 2 90))
			  ;; This function is faster on SBCL 1.0.18
			  ;; when we avoid calls to LOG and just use 1
			  ;; as the initial estimate, but only up to
			  ;; about 2^76 on one of my i386 machines and
			  ;; up to about 2^90 on one of my x86_64
			  ;; machines.
			  1
			  ;; This initial estimate is mathematically
			  ;; correct, but LOG is allowed to return
			  ;; inexact answers, hence the big estimate
			  ;; revision process above.
			  (1+ (floor (log n radix))))))))

(defun set-equal (list1 list2 &key key (test #'eql))
  "Returns a generalized boolean indicating whether LIST1 and LIST2,
when taken as unordered sets, contain exactly the same set of
items (according to TEST).  Duplicates either in LIST1 or in LIST2 are
ignored.

KEY, when not NIL, is a function of one argument, an element from
either LIST1 or LIST2.  KEY is used to produce an object
representative of the input element, to pass to TEST.  If KEY is NIL,
the elements from LIST1 and LIST2 are used directly (as if KEY is
#'IDENTITY).

TEST is a function of two arguments, the KEY of an element from LIST1
and the KEY of an element from LIST2 (or vice versa), which returns a
generalized boolean indicating whether the two objects should be
considered to be the same."
  ;; FIXME: optimization opportunity: if TEST is one of the HASH-TABLE
  ;; :TEST functions, make use of one.  Warn about potential memory
  ;; use, especially when applying KEY to each element in order to
  ;; populate the HT.
  (endp (set-exclusive-or list1 list2 :key key :test test)))

(defun parse-progn (body &key (allow-docstring-p t))
  "Given an implicit progn that may begin with an optional
documentation string and any number of declarations (in any order),
returns three values: the documentation string (or NIL if there isn't
any), a list of DECLARE expressions, and the rest of the list after
the initial docstring and declarations.  DECLARE expressions and a
documentation string are only recognized within the initial segment;
documentation strings may appear there only once."
  (let ((body-tail body)
	(docstring nil)
	(declarations '()))
    (loop
      (when (endp body-tail)
	(return))
      (let ((body-form (first body-tail)))
	(cond ((and (stringp body-form)
		    (not (endp (rest body-tail))))
	       (unless (and allow-docstring-p (null docstring))
		 (return))
	       (setf docstring (pop body-tail)))
	      ((and (listp body-form)
		    (not (endp body-form))
		    (eq 'declare (first body-form)))
	       (push (pop body-tail) declarations))
	      (t (return)))))
    (values docstring (nreverse declarations) body-tail)))

;;; Work around the fact that DEFINE-MODIFY-MACRO prescribes that the
;;; defined macro's first argument is the place, but we want it to be
;;; the last, and we don't want to reimplement stuff that
;;; DEFINE-MODIFY-MACRO gives us.
(defmacro push-append (list place)
  "Prepends the contents of LIST onto the list stored in PLACE.  The
resulting list is stored in PLACE and then returned."
  `(push-append% ,place ,list))
(define-modify-macro push-append% (list) swapped-append)
#+acl (proclaim '(inline swapped-append))
(defun swapped-append (l2 l1)
  (declare (type list l1))
  (append l1 l2))

(defmacro push-nconc (list place)
  "Destructively prepends the contents of LIST onto the list stored in
PLACE.  The resulting list is stored in PLACE and then returned."
  `(push-nconc% ,place ,list))
(define-modify-macro push-nconc% (list) swapped-nconc)
#+acl (proclaim '(inline swapped-nconc))
(defun swapped-nconc (l2 l1)
  (declare (type list l1))
  (nconc l1 l2))

(defmacro xor (&rest forms)
  "Returns the logical exclusive-or of the generalized boolean value
of each form: true if and only if there are an even number of forms
that return true.  Each form is evaluated once, and the forms are
evaluated in an unspecified order.  If no forms are given, NIL is
returned."
  (with-gensyms (state%)
    `(let ((,state% nil))
       (declare (type boolean ,state%))
       ,@(loop for form in forms
	       collecting `(when ,form (setf ,state% (not ,state%))))
       ,state%)))

(defmacro 1or (&rest forms)
  "Returns true if and only if exactly one of the given forms returns
true.  The single true value returned by one of the forms is returned.
Each form is evaluated at most once, and the forms are evaluated in an
unspecified order.  If no forms are given, NIL is returned."
  (with-gensyms (state% result% return%)
    `(block ,return%
       (let ((,state% nil) ,result%)
	 (declare (ignorable ,result%))
	 ,@(loop for form in forms
		 appending `((setf ,result% ,form)
			     (cond (,state%
				    (when ,result%
				      (return-from ,return% nil)))
				   (t (setf ,state% ,result%)))))
	 ,state%))))

(defmacro with-accessors* ((&rest accessors) instance-form &body body)
  "Like WITH-ACCESSORS, except that instead of giving (VARIABLE-NAME
ACCESSOR-NAME) for each accessor, you may give VARIABLE-NAME, in which
case ACCESSOR-NAME is derived from it."
  `(with-accessors
	 (,@(loop for spec in accessors collecting
		  (cond ((symbolp spec)
			 `(,spec ,spec))
			((and (listp spec)
			      (= 2 (length spec)))
			 spec)
			(t (error "Accessor specifier ~S is not a symbol or a ~
                                   list of the form (VARIABLE-NAME ~
                                   ACCESSOR-NAME)."
				  spec)))))
       ,instance-form
     ,@body))

(defun proper-list? (x)
  "Returns a generalized boolean indicating whether the given
object (of any actual type) is a proper list.  When it is, the true
value returned is the length of the list.

The differences between this and LIST-LENGTH are that this function
will accept objects of any type, and this function will return false
for dotted lists."
  ;; Code based on LIST-LENGTH example implementation in CLHS.
  (when (listp x)
    (loop for n from 0 by 2
	  for fast-tail = x then (cddr fast-tail)
	  for slow-tail = x then (cdr slow-tail)
	  doing
	  ;; If the next iteration of the fast tail (or the inbetween
	  ;; tail) is not a list, then we're on a dotted list.  No
	  ;; need to check the slow tail.
	  (unless (and (listp (cdr fast-tail))
		       (listp (cddr fast-tail)))
	    (return nil))
	  ;; If fast pointer hits the end, return the count.
	  (when (endp fast-tail)
	    (return n))
	  (when (endp (cdr fast-tail))
	    (return (1+ n)))
	  ;; If fast pointer eventually equals slow pointer, then we
	  ;; must be stuck in a circular list.
	  ;; 
	  ;; (A deeper property is the converse: if we are stuck in a
	  ;; circular list, then eventually the fast pointer will
	  ;; equal the slow pointer.  That fact justifies this
	  ;; implementation.)
	  (when (and (eq fast-tail slow-tail)
		     (plusp n))
	    (return nil)))))

(defmacro doseq ((var sequence-form &optional (result-form)) &body body)
  "Iterates over the elements of the sequence produced by evaluating
SEQUENCE-FORM similarly to DOLIST.

BODY may be included in the expansion more than once, for
specialization on the different types of sequences without the
overhead of function calls."
  (with-gensyms (next% size% exhausted?%)
    `(with-sequence-iterator (,next% ,size% ,exhausted?% ,sequence-form)
       (let (,var)
	 (loop
	   (when (,exhausted?%) (return ,result-form))
	   (setf ,var (,next%))
	   (progn ,@body))))))
