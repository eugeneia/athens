(in-package #:jpl-queues)

;;; Technique copied from R. Scott McIntire's queue.lisp (RSM.QUEUE),
;;; but it probably goes back way further than that.

(defclass unbounded-fifo-queue (queue)
  ((buffer :type list :initform '() :reader buffer
	   :documentation "A list of elements, oldest first, that have
yet to be read.

The cells of the list do not share structure with anything outside the
queue.")
   (last-cell :type (or cons null) :initform nil
	      :documentation "The last cons cell of BUFFER, or NIL if
BUFFER is empty.")
   (size :type (integer 0) :initform 0 :reader size
	 :documentation "The number of elements in BUFFER."))
  (:documentation "An unbounded FIFO queue.

Space usage is proportional to the number of queued elements at any
given point in time.  Conses for each enqueued element."))

(defmethod capacity ((queue unbounded-fifo-queue))
  nil)

(defmethod enqueue (object (queue unbounded-fifo-queue))
  (with-slots (buffer last-cell size) queue
    (setf last-cell
	  (if (null last-cell)
	      (setf buffer (list object))
	      (setf (rest last-cell) (list object))))
    (incf size)))

(defmethod dequeue ((queue unbounded-fifo-queue))
  (with-slots (buffer last-cell size) queue
    (prog1 (pop buffer)
      (decf size)
      (when (endp buffer)
	(setf last-cell nil)))))

(defmethod dequeue-object-if (predicate (queue unbounded-fifo-queue)
			      &key (key #'identity))
  (with-slots (buffer last-cell size) queue
    (setf buffer (delete-if predicate buffer :key key)
	  last-cell (last buffer)
	  size (length buffer))))
