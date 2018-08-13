(in-package #:jpl-queues)

(defclass bounded-fifo-queue (queue)
  ((buffer :type vector
           :documentation "A ring buffer of elements, oldest first,
that have yet to be read.")
   (size :type jpl-util:array-dimension :initform 0 :reader size
	 :documentation "The number of elements queued in BUFFER.")
   (start :type jpl-util:array-index :initform 0
	  :documentation "The index into BUFFER of its oldest
element."))
  (:documentation "A bounded FIFO queue.  The queue capacity (a
positive integer less than ARRAY-DIMENSION-LIMIT) must be specified
with the :CAPACITY keyword parameter.

Space usage is constant after instantiation, in proportion to the
capacity.  Conses little, if at all, for any operation."))

(defmethod initialize-instance :after ((queue bounded-fifo-queue)
				       &key (capacity nil capacity-p)
				       &allow-other-keys)
  (unless capacity-p
    (error "Must specify queue capacity with :CAPACITY keyword ~
            parameter when instantiating a ~S."
	   (type-of queue)))
  (check-type capacity jpl-util:array-dimension
	      "a queue capacity small enough to be an array dimension")
  (when (zerop capacity)
    (error "A 0-length queue would only block."))
  (setf (slot-value queue 'buffer) (make-array capacity)))

(defmethod print-object ((queue bounded-fifo-queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (format stream "capacity ~D" (capacity queue))))

(defmethod capacity ((queue bounded-fifo-queue))
  (with-slots (buffer) queue
    (array-dimension buffer 0)))

(defmethod enqueue (object (queue bounded-fifo-queue))
  (with-slots (buffer start size) queue
    (setf (aref buffer (mod (+ start size) (capacity queue)))
	  object)
    (incf size)))

(defmethod dequeue ((queue bounded-fifo-queue))
  (with-slots (buffer start size) queue
    (prog1 (aref buffer start)
      ;; Old elements can be big and otherwise dead.  Don't hold onto
      ;; them, or else the GC will consider them to be alive.
      (setf (aref buffer start) nil)
      (setf start (mod (1+ start) (capacity queue)))
      (decf size))))

(defmethod dequeue-object-if (predicate (queue bounded-fifo-queue)
			      &key (key #'identity))
  (with-slots (buffer start size) queue
    (flet ((physical-index (i)
	     (mod (+ start i) (capacity queue))))
      (loop for input-pos from 0 below size
	    for output-pos = 0 then (- input-pos offset)
	    for element = (aref buffer (physical-index input-pos))
	    for delete? = (funcall predicate (funcall key element))
	    counting delete? into offset
	    unless (or delete? (= input-pos output-pos))
	    doing (setf (aref buffer (physical-index output-pos))
			element)
	    finally (loop for del-pos from (- size offset) below size
			  doing (setf (aref buffer (physical-index del-pos)) nil))
	    finally (decf size offset)))))

(defun test-bounded-fifo-queue-dequeue-object-if (list-length)
  (flet ((mod4? (x)
	   ;; Pattern: X...X...X...
	   (zerop (mod x 4)))
	 (not-mod4? (x)
	   ;; Pattern: .XXX.XXX.XXX
	   (plusp (mod x 4)))
	 (group3-1? (x)
	   ;; Pattern: ...XXX...XXX
	   (oddp (floor x 3)))
	 (group3-2? (x)
	   ;; Pattern: XXX...XXX...
	   (evenp (floor x 3)))
	 (always (x)
	   (declare (ignore x))
	   t)
	 (never (x)
	   (declare (ignore x))
	   nil))
    (let ((objects (loop for i below list-length collecting i))
	  (queue-capacities (loop for queue-capacity from list-length upto (* 2 list-length)
				  collecting queue-capacity))
	  (tests (list #'evenp #'oddp #'mod4? #'not-mod4? #'group3-1?
		       #'group3-2? #'always #'never)))
      (dolist (queue-capacity queue-capacities)
	(dotimes (queue-start-val queue-capacity)
	  (dolist (test tests)
	    (let ((queue (make-instance 'bounded-fifo-queue :capacity queue-capacity)))
	      ;; We'll test to ensure that everything ends up NIL, so
	      ;; make sure each position is defined.
	      (fill (slot-value queue 'buffer) nil)
	      ;; Manipulate the start pointer.
	      (dotimes (i queue-start-val)
		(enqueue queue 'junk)
		(dequeue queue))
	      ;; Fill queue with test values.
	      (dolist (object objects)
		(assert (not (full? queue)))
		(enqueue queue object))
	      ;; The moment of truth.
	      (dequeue-object-if test queue)
	      (assert (equal (remove-if test objects)
			     (loop until (empty? queue)
				   collecting (dequeue queue))))
	      ;; Ensure old places were NILed out.
	      (assert (= (count-if #'null (slot-value queue 'buffer))
			 (- queue-capacity list-length))))))))))
