(in-package #:common-lisp-user)

(defpackage #:jpl-queues
  (:export #:queue
	   #:bounded-fifo-queue
	   #:lossy-bounded-fifo-queue
	   #:unbounded-fifo-queue
	   #:unbounded-random-queue
	   #:synchronized-queue
	   
	   #:empty?
	   #:full?
	   #:size
	   #:capacity
	   #:enqueue
	   #:dequeue
	   #:dequeue-object
	   #:dequeue-object-if)
  (:use #:common-lisp))
