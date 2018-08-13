(in-package #:jpl-queues)

;;; This could be made to be a mixin class, for use with other bounded
;;; queues.
(defclass lossy-bounded-fifo-queue (bounded-fifo-queue)
  ()
  (:documentation "A bounded FIFO queue that throws away old entries
when ENQUEUE is called and the queue is full.  A queue capacity (a
positive integer less than ARRAY-DIMENSION-LIMIT) must be specified
with the :CAPACITY keyword parameter.

Space usage is constant after instantiation, in proportion to the
capacity.  Conses little, if at all, for any operation."))

(defmethod full? ((queue lossy-bounded-fifo-queue))
  nil)

(defmethod enqueue :before (object (queue lossy-bounded-fifo-queue))
  (when (= (size queue) (capacity queue))
    (dequeue queue)))
