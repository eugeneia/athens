(in-package :percent)

(defun shrink-vector (vector size)
  #+sbcl
  (sb-kernel:%shrink-vector vector size)
  #+ccl
  (ccl::shrink-vector vector size)
  #-(or sbcl ccl)
  (subseq vector 0 size))

(declaim (inline char-pair-octet))
(defun char-pair-octet (c1 c2)
  (let ((h (digit-char-p c1 16))
        (l (digit-char-p c2 16)))
    (if (and h l)
        (+ (ash h 4) l)
        nil)))
