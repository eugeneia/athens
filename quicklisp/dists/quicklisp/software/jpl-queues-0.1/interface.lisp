(in-package #:jpl-queues)

(defclass queue ()
  ()
  (:documentation "Abstract class for queues.

Unless noted otherwise, subclasses do no synchronization.  It is the
client's responsibility to ensure no two operations occur
simultaneously.

The usual time complexity for each operation is documented in the
generic function for the operation.  Per-method exceptions should be
documented."))

;;; Generic functions for instances.

(defgeneric empty? (queue)
  (:documentation "Returns a boolean indicating whether the given
QUEUE cannot have any more elements dequeued from it.  When this
function returns false, it is safe to call DEQUEUE.

Most implementations will take constant time.")
  (:method ((queue queue))
    (zerop (size queue))))

(defgeneric full? (queue)
  (:documentation "Returns a boolean indicating whether the given
QUEUE cannot have any more elements enqueued to it.  When this
function returns false, it is safe to call ENQUEUE.

This is subtly different than saying its SIZE is equal to its
CAPACITY: a lossy queue may have a capacity, and it may be \"full\" in
that sense, but it will still accept new elements to be enqueued.

Most implementations will take constant time.")
  (:method ((queue queue))
    (let ((capacity (capacity queue)))
      (and (not (null capacity))
	   (= capacity (size queue))))))

(defgeneric size (queue)
  (:documentation "Returns the number of elements stored in QUEUE.

Most implementations will take constant time."))

(defgeneric capacity (queue)
  (:documentation "Returns the number of elements that can be stored
at once in the given QUEUE, or NIL if there is no limit.

Most implementations will take constant time."))

(defgeneric enqueue (object queue)
  (:documentation "Writes OBJECT to QUEUE.

After this function returns, EMPTY? must return false.

It is the caller's responsibility to determine whether QUEUE has room
for another element.  The consequences are undefined if it doesn't.

Most implementations will take constant time."))

(defgeneric dequeue (queue)
  (:documentation "Removes an element from QUEUE, returning the
element.

After this function returns, FULL? must return false.

It is the caller's responsibility to determine whether QUEUE has an
element available for reading.  The consequences are undefined if it
doesn't.

Most implementations will take constant time."))

(defgeneric dequeue-object (object queue &key test key)
  (:documentation "Prematurely dequeues OBJECT from QUEUE.

TEST is a function of two arguments that is used to compare OBJECT
with the result of KEY applied to each enqueued element.  When TEST
returns true, the element is deleted.  Zero or more elements may be
matched and removed.

If anything was actually removed, FULL? must return false.

Most implementations will have the same time complexity as
DELETE-OBJECT-IF.")
  (:method (object (queue queue) &key (test #'eql) (key #'identity))
    (dequeue-object-if (lambda (other-object)
			 (funcall test object other-object))
		       queue :key key)))

(defgeneric dequeue-object-if (predicate queue &key key)
  (:documentation "Prematurely dequeues elements from QUEUE that
PREDICATE returns true for.

PREDICATE is called with the result of KEY applied to each enqueued
element.  When PREDICATE returns true, the element is deleted.  Zero
or more elements may be matched and removed.

If anything was actually removed, FULL? must return false.

Most implementations will take time in linear proportion to the number
of enqueued elements."))
