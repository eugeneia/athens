;; test.lisp

(defpackage :garbage-pools.test
  (:use #:cl #:garbage-pools #:lift)
  (:nicknames #:gp.test)
  (:export #:run-garbage-pools-tests))

(in-package #:garbage-pools.test)


(deftestsuite garbage-pools-test () ())

;;; test-cleanup-pool

(addtest (garbage-pools-test)
  test-cleanup-pool
  (ensure-same '(1 2 3 4)
               (let ((list nil)
                     (pool (make-instance 'pool)))
                 (loop for x from 1 to 4 do
                      (cleanup-register x (lambda (obj) (push obj list)) pool))
                 (cleanup-pool pool)
                 list)))

;;; test-with-garbage-pool-1

(addtest (garbage-pools-test)
  test-with-cleanup-pool-1
  (ensure-same '(1 2 3 4 5)
               (let ((list nil))
                 (with-garbage-pool ()
                   (loop for x from 1 to 5 do
                        (cleanup-register x (lambda (obj) (push obj list)))))
                 list)))

;;; test-with-cleanup-pool-2

(addtest (garbage-pools-test)
  test-with-cleanup-pool-2
  (ensure-same '(1 2 3 4 5)
               (let ((list nil))
                 (with-garbage-pool (mypool)
                   (loop for x from 1 to 5 do
                        (cleanup-register x (lambda (obj) (push obj list)) mypool)))
                 list)))

;;; test-cleanup-object-1

(addtest (garbage-pools-test)
  test-cleanup-object-1
  (ensure-same '((3 . 3) (1 . 1))
               (let ((res nil)
                     (res2 nil)
                     (data '((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4))))
                 (with-garbage-pool ()
                   (loop for x in data do
                        (cleanup-register x (lambda (obj) (push obj res))))
                   (cleanup-object (nth 1 data))
                   (cleanup-object (nth 3 data))
                   (setq res2 res)
                   (setq res nil))
                 res2)))

;;; test-cleanup-object-2

(addtest (garbage-pools-test)
  test-cleanup-object-2
  (ensure-same '((0 . 0) (2 . 2) (4 . 4))
               (let ((res nil)
                     (data '((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4))))
                 (with-garbage-pool ()
                   (loop for x in data do
                        (cleanup-register x (lambda (obj) (push obj res))))
                   (cleanup-object (nth 1 data))
                   (cleanup-object (nth 3 data))
                   (setq res nil))
                 res)))


;;; test-object-register-and-defcleanup

(defclass test-class ()
  ((content :initarg :content :initform nil :accessor content)))

(defvar *cesspool*)

(defcleanup test-class (lambda (obj) (push (content obj) *cesspool*)))

(addtest (garbage-pools-test)
  test-object-register-and-defcleanup-1
  (ensure-same '("Hello" "world")
               (let ((*cesspool* nil))
                 (with-garbage-pool ()
                   (object-register (make-instance 'test-class :content "Hello"))
                   (object-register (make-instance 'test-class :content "world")))
                 *cesspool*)))
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-garbage-pools-tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-garbage-pools-tests ()
  (run-tests :suite 'garbage-pools-test))