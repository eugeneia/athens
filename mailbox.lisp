;;;; Mailbox FIFO queue implementation based on JPL-QUEUES.

(in-package :erlangen.mailbox)

(defstruct (mailbox (:constructor make-mailbox%))
  "Mailbox structure."
  (queue (error "QUEUE must be supplied.") :type bounded-fifo-queue)
  (priority (error "PRIORITY must be supplied.") :type unbounded-fifo-queue)
  (open? t :type symbol)
  (lock (make-lock "erlangen.mailbox"))
  (enqueued (make-semaphore))
  (messages-dequeued 0 :type (unsigned-byte 60))
  (messages-dropped 0 :type (unsigned-byte 60)))

(defun make-mailbox (size)
  "Return a new empty mailbox of SIZE."
  (make-mailbox%
   :queue (make-instance 'bounded-fifo-queue :capacity size)
   :priority (make-instance 'unbounded-fifo-queue)))

(defun enqueue-message (message mailbox)
  "Attempt to enqueue MESSAGE in MAILBOX."
  (with-slots (queue open? lock enqueued messages-dropped) mailbox
    (with-lock-grabbed (lock)
      (if (and open? (not (full? queue)))
          (progn (enqueue message queue)
                 (signal-semaphore enqueued))
          (incf messages-dropped))))
  (values))

(defun enqueue-priority (message mailbox)
  "Attempt to enqueue priority MESSAGE in MAILBOX. Fails if MAILBOX is closed,
but does *not* signal an error."
  (with-slots (priority open? lock enqueued messages-dropped) mailbox
    (with-lock-grabbed (lock)
      (if open?
          (progn (enqueue message priority)
                 (signal-semaphore enqueued))
          (incf messages-dropped))))
  (values))

(defun empty-p (mailbox)
  "Predicate to test if MAILBOX is empty."
  (with-slots (queue priority lock) mailbox
    (with-lock-grabbed (lock)
      (and (empty? queue) (empty? priority)))))

(defun dequeue-message (mailbox &key timeout)
  "Return the next message in MAILBOX. Blocks depending on TIMEOUT. Only one
process (the “owner”) may call DEQUEUE-MESSAGE on a given `mailbox'."
  (with-slots (queue priority lock enqueued messages-dequeued) mailbox
    (case timeout
      ;; TIMEOUT = nil: wait for new message indefinitely
      ((nil)     (wait-on-semaphore enqueued))
      ;; TIMEOUT = 0, signal `timeout' immediately
      (0         (unless (try-semaphore enqueued)
                   (error 'timeout)))
      ;; TIMEOUT = n: wait up to n seconds for new message
      (otherwise (unless (timed-wait-on-semaphore enqueued timeout)
                   (error 'timeout))))
    (with-lock-grabbed (lock)
      (incf messages-dequeued)
      (if (empty? priority)
          (dequeue queue)
          (dequeue priority)))))

(defun close-mailbox (mailbox)
  "Close MAILBOX."
  (with-lock-grabbed ((mailbox-lock mailbox))
    (setf (mailbox-open? mailbox) nil)))
