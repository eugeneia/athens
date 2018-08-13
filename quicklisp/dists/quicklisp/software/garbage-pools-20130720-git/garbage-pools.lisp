;; garbage-pools.lisp

(defpackage :garbage-pools
  (:use #:cl)
  (:export #:pool
           #:with-garbage-pool
           #:cleanup-register
           #:cleanup-pool
           #:cleanup-object
           #:cancel-object-cleanup
           #:object-register
           #:defcleanup))

(in-package #:garbage-pools)

(defvar *pool*)

;;; pool

(defclass pool ()
  ((register-pairs :initform nil :accessor register-pairs)))

;;; cleanup-register

(defun cleanup-register (object cleanup-fun &optional (pool *pool*))
  (push (cons object cleanup-fun)
        (register-pairs pool))
  object)

;;; cleanup-pool

(defun cleanup-pool (&optional (pool *pool*))
  (dolist (pair (register-pairs pool))
    (let ((obj (car pair))
          (cleanup-fun (cdr pair)))
      (if (and obj cleanup-fun)
          (funcall cleanup-fun obj)))
    (setf (register-pairs pool) nil)))

;;; cleanup-object

(defun cleanup-object (object &optional (pool *pool*))
  (let ((pair (find object (register-pairs pool) :key #'car :test #'eq)))
    (if (and pair (car pair) (cdr pair))
        (funcall (cdr pair) (car pair)))
    (delete pair (register-pairs pool))))

;;; cancel-cleanup

(defun cancel-object-cleanup (object &optional (pool *pool*))
  (let ((pair (find object (register-pairs pool) :key #'car :test #'eq)))
    (if pair
        (delete pair (register-pairs pool)))))
  

;;; with-garbage-pool

(defmacro with-garbage-pool ((&optional (var '*pool*)) &body body)
  `(let ((,var (make-instance 'pool)))
     (unwind-protect
          (progn ,@body)
       (cleanup-pool ,var))))

;;; object-register

(defgeneric object-register (object &optional pool))

(defmethod object-register ((empty (eql nil)) &optional (pool garbage-pools::*pool*))
  (declare (ignore pool)))
  

;;; defcleanup

(defmacro defcleanup (class cleanup-fun)
  `(defmethod garbage-pools:object-register ((object ,class) &optional (pool garbage-pools::*pool*))
     (garbage-pools:cleanup-register object ,cleanup-fun pool)))

(defcleanup pool #'cleanup-pool)

(defcleanup stream #'close)
     

  
