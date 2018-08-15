(in-package #:jpl-util)

(defun vector-delete (vector index &optional (nil-p t))
  "Destructively removes the element specified at INDEX from VECTOR in
constant time.  VECTOR must have a fill pointer; it is updated.  INDEX
must be less than the length of VECTOR (according to its fill
pointer).

When NIL-P is true or not given, elements made to be
unreferenced (outside the active region denoted by the fill-pointer)
are overwritten with NIL, to prevent holding onto potentially big
objects, which would keep them from garbage collection indefinitely.
When NIL-P is false, the set of elements within VECTOR will remain the
same--only reordering and fill-pointer manipulation will occur.
Generally, NIL-P should be false only when VECTOR shares structure
with another vector (via displacement) or its fill-pointer will later
be artificially manipulated in order to recover deleted objects.

The order of elements within VECTOR should be mostly insignificant,
such as when VECTOR represents a set of objects, because elements are
reordered.  Specifically, the last element of VECTOR and the element
at INDEX are swapped.  Therefore, after returning, the positions of
the element at INDEX and the element just past the fill-pointer may be
different (with the exception that the element just past the
fill-pointer will be NIL when NIL-P is true); other elements will
retain their position.  (This apparent implementation detail is
explicitly specified to allow for specific algorithms and bookkeeping
that depends on the indices of VECTOR.)"
  (declare (type vector vector)
	   (type array-index index))
  (let ((old-element (unless nil-p (aref vector index)))
	(last-index (decf (fill-pointer vector))))
    (setf (aref vector index) (aref vector last-index)
	  (aref vector last-index) old-element))
  (values))

(defun vector-delete-range (vector &key (start 0) end)
  "Destructively removes a subsequence of elements, specified by START
and END, from VECTOR, in time linear proportional to the length of the
subsequence.  VECTOR must have a fill pointer; it is updated.  START
and END must be bounding indices with the length of VECTOR (according
to its fill pointer).

The order of elements within VECTOR may be changed starting with START
and up to the end of the VECTOR.  Therefore, this function is best
used for situations where a vector is considered to be a set, and the
order of its elements doesn't matter."
  (declare (type vector vector)
	   (type array-index start)
	   (type (or array-index null) end))
  (let* ((old-fp (fill-pointer vector))
	 (end (if (null end) old-fp end))
	 (new-fp (- old-fp (- end start))))
    ;; Move the last N elements of the vector into the requested
    ;; subsequence (where N is the subsequence length).  If some of
    ;; the last N are to be deleted, exclude those from the move.
    (replace vector vector
	     :start1 start :end1 end
	     :start2 (max new-fp end) :end2 old-fp)
    ;; And then fill the last N with NIL.
    (fill vector nil :start new-fp :end old-fp)
    (setf (fill-pointer vector) new-fp))
  (values))

;;; The functions below are just for internal testing.

(defun vector-equal (v1 v2)
  (and (= (length v1)
	  (length v2))
       (loop for e1 across v1
	     for e2 across v2
	     repeat (length v1) ; Honor fill-pointer.
	     unless (= e1 e2)
	     doing (return nil)
	     finally (return t))))

(defun test-vector-delete-functions (&key (count 1000) (length 10))
  (dotimes (i count)
    (let* ((vector (coerce (loop for i from 1 repeat length collecting i) 'vector))
	   (sub-length (random (1+ length)))
	   (sub-start (random (1+ (- length sub-length))))
	   (sub-end (+ sub-start sub-length))
	   (vector/no-sub (concatenate 'vector
				       (subseq vector 0 sub-start)
				       (subseq vector sub-end))))
      (flet ((test-fn (fn)
	       (let ((vector-copy (make-array length :fill-pointer length
					      :adjustable t
					      :initial-contents vector))
		     (grow-length (random 3)))
		 (adjust-array vector-copy (+ length grow-length))
		 (funcall fn vector-copy sub-start sub-end vector/no-sub))))
	(test-fn #'test-vector-delete)
	(test-fn #'test-vector-delete-range)))))

(defun test-vector-delete-range (vector sub-start sub-end expected)
  (vector-delete-range vector :start sub-start :end sub-end)
  (setf vector (nsort vector #'<))
  (unless (vector-equal vector expected)
    (error "VECTOR-DELETE-RANGE failed: [~D,~D)" sub-start sub-end)))

(defun test-vector-delete (vector sub-start sub-end expected)
  (let ((subvector (subseq vector sub-start sub-end)))
    (shuffle! vector)
    (loop for elt across subvector
	  for pos = (position elt vector :test #'=)
	  doing (vector-delete vector pos))
    (setf vector (nsort vector #'<)))
  (unless (vector-equal vector expected)
    (error "VECTOR-DELETE failed: [~D,~D)" sub-start sub-end)))
