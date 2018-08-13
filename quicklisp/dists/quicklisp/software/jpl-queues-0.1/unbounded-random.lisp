(in-package #:jpl-queues)

(defclass unbounded-random-queue (queue)
  ((buffer :type vector
           :documentation "A buffer of elements, in unspecified order,
that have yet to be read."))
  (:documentation "A virtually unbounded, random-order
queue.  (Strictly speaking, the queue size is limited by
ARRAY-DIMENSION-LIMIT.)

Space usage is proportional to the peak number of queued elements over
the lifetime of the queue.  An initial queue capacity (a positive
integer less than ARRAY-DIMENSION-LIMIT) may optionally be specified
with the :CAPACITY keyword parameter; the capacity will be grown as
required.  Conses for an enqueued element only when the current queue
capacity is reached and needs to be extended."))

(defmethod initialize-instance :after ((queue unbounded-random-queue)
				       &key (capacity 0 capacity-p)
				       &allow-other-keys)
  (when capacity-p
    (check-type capacity jpl-util:array-dimension
		"a queue capacity small enough to be an array dimension"))
  (setf (slot-value queue 'buffer)
	(make-array capacity :fill-pointer 0 :adjustable t)))

(defmethod size ((queue unbounded-random-queue))
  (with-slots (buffer) queue
    (fill-pointer buffer)))

(defmethod capacity ((queue unbounded-random-queue))
  (1- array-dimension-limit))

(defmethod enqueue (object (queue unbounded-random-queue))
  (with-slots (buffer) queue
    (vector-push-extend object buffer)))

(defmethod dequeue ((queue unbounded-random-queue))
  (with-slots (buffer) queue
    (let ((index (random (fill-pointer buffer))))
      (prog1 (aref buffer index)
	(jpl-util:vector-delete buffer index)))))

(defmethod dequeue-object-if (predicate (queue unbounded-random-queue)
			      &key (key #'identity))
  (with-slots (buffer) queue
    (loop with i = 0
	  while (< i (fill-pointer buffer))
	  doing (cond ((funcall predicate (funcall key (aref buffer i)))
		       (jpl-util:vector-delete buffer i))
		      (t (incf i))))))
