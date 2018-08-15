(in-package #:jpl-queues)

(defclass synchronized-queue (queue)
  ((queue :type queue :initarg :queue
	  :initform (error "Must specify :QUEUE.")
	  :documentation "The wrapped QUEUE for which operations are
synchronized.")
   (lock :initform (bt:make-lock)
	 :documentation "The LOCK which must be held during each
operation.")
   (enqueued :initform (bt:make-condition-variable)
	     :documentation "Condition variable that is notified each
time an element has been enqueued.")
   (dequeued :initform (bt:make-condition-variable)
	     :documentation "Condition variable that is notified each
time an element has been dequeued."))
  (:documentation "Wraps another QUEUE (given by the :QUEUE keyword
parameter), synchronizing operations for safe multithreaded access.
After instantiating this queue, the wrapped QUEUE must not be directly
used for the duration of this queue.

When ENQUEUE is called on this QUEUE and the wrapped QUEUE is full,
blocks until it is no longer full.  When DEQUEUE is called on this
QUEUE and the wrapped QUEUE is empty, blocks until it is no longer
empty.  This blocking also ensures that the internal state of the
wrapped QUEUE won't become corrupt by calling DEQUEUE when empty or
ENQUEUE when full.

Operations that should return no useful value will return no values.

The time and space complexity for this queue and all its operations
are equal to those of the wrapped QUEUE, plus any locking overhead
imposed by the system, except that ENQUEUE and DEQUEUE may block
indefinitely."))

(defmethod print-object ((queue synchronized-queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (format stream "for ~S" (slot-value queue 'queue))))

(defmethod empty? ((queue synchronized-queue))
  (with-slots ((wrapped-queue queue) lock) queue
    (bt:with-lock-held (lock)
      (empty? wrapped-queue))))

(defmethod full? ((queue synchronized-queue))
  (with-slots ((wrapped-queue queue) lock) queue
    (bt:with-lock-held (lock)
      (full? wrapped-queue))))

(defmethod size ((queue synchronized-queue))
  (with-slots ((wrapped-queue queue) lock) queue
    (bt:with-lock-held (lock)
      (size wrapped-queue))))

(defmethod capacity ((queue synchronized-queue))
  (with-slots ((wrapped-queue queue) lock) queue
    (bt:with-lock-held (lock)
      (capacity wrapped-queue))))

(defmethod enqueue (object (queue synchronized-queue))
  (with-slots ((wrapped-queue queue) lock enqueued dequeued) queue
    (bt:with-lock-held (lock)
      (loop while (full? wrapped-queue)
	    doing (bt:condition-wait dequeued lock))
      (enqueue object wrapped-queue)
      (bt:condition-notify enqueued)))
  (values))

(defmethod dequeue ((queue synchronized-queue))
  (with-slots ((wrapped-queue queue) lock enqueued dequeued) queue
    (bt:with-lock-held (lock)
      (loop while (empty? wrapped-queue)
	    doing (bt:condition-wait enqueued lock))
      (prog1 (dequeue wrapped-queue)
	(bt:condition-notify dequeued)))))

(defmethod dequeue-object (object (queue synchronized-queue)
			   &key (test #'eql) (key #'identity))
  (with-slots ((wrapped-queue queue) lock dequeued) queue
    (bt:with-lock-held (lock)
      (let ((old-size (size wrapped-queue)))
	(dequeue-object object wrapped-queue :test test :key key)
	;; Notify once for each element that was effectively dequeued.
	;; 
	;; FIXME: Try modifying BORDEAUX-THREADS to take a
	;; NOTIFY-COUNT parameter that takes care of this for us,
	;; which on most threading implementations would probably be
	;; in a smarter way than this.
	(loop repeat (- old-size (size wrapped-queue))
	      doing (bt:condition-notify dequeued)))))
  (values))

(defmethod dequeue-object-if (predicate (queue synchronized-queue)
			      &key (key #'identity))
  (with-slots ((wrapped-queue queue) lock dequeued) queue
    (bt:with-lock-held (lock)
      (let ((old-size (size wrapped-queue)))
	(dequeue-object-if predicate wrapped-queue :key key)
	;; Notify once for each element that was effectively dequeued.
	;; 
	;; FIXME: Try modifying BORDEAUX-THREADS to take a
	;; NOTIFY-COUNT parameter that takes care of this for us,
	;; which on most threading implementations would probably be
	;; in a smarter way than this.
	(loop repeat (- old-size (size wrapped-queue))
	      doing (bt:condition-notify dequeued)))))
  (values))
