;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; poly1305.lisp -- RFC 7539 poly1305 message authentication code


(in-package :crypto)


(defclass poly1305 ()
  ((accumulator :accessor poly1305-accumulator
                :initform (make-array 5 :element-type '(unsigned-byte 32))
                :type (simple-array (unsigned-byte 32) (5)))
   (r :accessor poly1305-r
      :initform (make-array 5 :element-type '(unsigned-byte 32))
      :type (simple-array (unsigned-byte 32) (5)))
   (s :accessor poly1305-s
      :initform (make-array 4 :element-type '(unsigned-byte 32))
      :type (simple-array (unsigned-byte 32) (4)))
   (buffer :accessor poly1305-buffer
           :initform (make-array 16 :element-type '(unsigned-byte 8))
           :type (simple-array (unsigned-byte 8) (16)))
   (buffer-length :accessor poly1305-buffer-length
                  :initform 0
                  :type (integer 0 16))))

(defun make-poly1305 (key)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (unless (= (length key) 32)
    (error 'invalid-mac-parameter
           :mac-name 'poly1305
           :message "The key length must be 32 bytes"))
  (make-instance 'poly1305 :key key))

(defmethod shared-initialize :after ((mac poly1305) slot-names &rest initargs &key key &allow-other-keys)
  (declare (ignore slot-names initargs)
           (type (simple-array (unsigned-byte 8) (32)) key))
  (let ((accumulator (poly1305-accumulator mac))
        (r (poly1305-r mac))
        (s (poly1305-s mac)))
    (declare (type (simple-array (unsigned-byte 32) (5)) accumulator r)
             (type (simple-array (unsigned-byte 32) (4)) s))
    (fill accumulator 0)
    (setf (aref r 0) (logand (ub32ref/le key 0) #x3ffffff)
          (aref r 1) (logand (mod32ash (ub32ref/le key 3) -2) #x3ffff03)
          (aref r 2) (logand (mod32ash (ub32ref/le key 6) -4) #x3ffc0ff)
          (aref r 3) (logand (mod32ash (ub32ref/le key 9) -6) #x3f03fff)
          (aref r 4) (logand (mod32ash (ub32ref/le key 12) -8) #x00fffff))
    (setf (aref s 0) (ub32ref/le key 16)
          (aref s 1) (ub32ref/le key 20)
          (aref s 2) (ub32ref/le key 24)
          (aref s 3) (ub32ref/le key 28))
    (setf (poly1305-buffer-length mac) 0)
    mac))

(defun poly1305-process-full-blocks (accumulator r data start remaining final)
  (declare (type (simple-array (unsigned-byte 32) (5)) accumulator r)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start remaining)
           (type boolean final)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let* ((hibit (if final 0 #.(ash 1 24)))
         (r0 (aref r 0))
         (r1 (aref r 1))
         (r2 (aref r 2))
         (r3 (aref r 3))
         (r4 (aref r 4))
         (s1 (mod32* r1 5))
         (s2 (mod32* r2 5))
         (s3 (mod32* r3 5))
         (s4 (mod32* r4 5))
         (h0 (aref accumulator 0))
         (h1 (aref accumulator 1))
         (h2 (aref accumulator 2))
         (h3 (aref accumulator 3))
         (h4 (aref accumulator 4))
         (d0 0)
         (d1 0)
         (d2 0)
         (d3 0)
         (d4 0)
         (c 0))
    (declare (type (unsigned-byte 32) hibit r0 r1 r2 r3 r4 s1 s2 s3 s4 h0 h1 h2 h3 h4 c)
             (type (unsigned-byte 64) d0 d1 d2 d3 d4))
    (loop while (>= remaining 16) do
      (setf h0 (mod32+ h0 (logand (ub32ref/le data start) #x3ffffff))
            h1 (mod32+ h1 (logand (mod32ash (ub32ref/le data (+ start 3)) -2) #x3ffffff))
            h2 (mod32+ h2 (logand (mod32ash (ub32ref/le data (+ start 6)) -4) #x3ffffff))
            h3 (mod32+ h3 (logand (mod32ash (ub32ref/le data (+ start 9)) -6) #x3ffffff))
            h4 (mod32+ h4 (logior (mod32ash (ub32ref/le data (+ start 12)) -8) hibit)))
      (setf d0 (mod64+ (mod64* h0 r0)
                       (mod64+ (mod64* h1 s4)
                               (mod64+ (mod64* h2 s3)
                                       (mod64+ (mod64* h3 s2)
                                               (mod64* h4 s1)))))
            d1 (mod64+ (mod64* h0 r1)
                       (mod64+ (mod64* h1 r0)
                               (mod64+ (mod64* h2 s4)
                                       (mod64+ (mod64* h3 s3)
                                               (mod64* h4 s2)))))
            d2 (mod64+ (mod64* h0 r2)
                       (mod64+ (mod64* h1 r1)
                               (mod64+ (mod64* h2 r0)
                                       (mod64+ (mod64* h3 s4)
                                               (mod64* h4 s3)))))
            d3 (mod64+ (mod64* h0 r3)
                       (mod64+ (mod64* h1 r2)
                               (mod64+ (mod64* h2 r1)
                                       (mod64+ (mod64* h3 r0)
                                               (mod64* h4 s4)))))
            d4 (mod64+ (mod64* h0 r4)
                       (mod64+ (mod64* h1 r3)
                               (mod64+ (mod64* h2 r2)
                                       (mod64+ (mod64* h3 r1)
                                               (mod64* h4 r0))))))
      (setf c (logand (mod64ash d0 -26) #xffffffff)
            h0 (logand d0 #x3ffffff))
      (setf d1 (mod64+ d1 c)
            c (logand (mod64ash d1 -26) #xffffffff)
            h1 (logand d1 #x3ffffff))
      (setf d2 (mod64+ d2 c)
            c (logand (mod64ash d2 -26) #xffffffff)
            h2 (logand d2 #x3ffffff))
      (setf d3 (mod64+ d3 c)
            c (logand (mod64ash d3 -26) #xffffffff)
            h3 (logand d3 #x3ffffff))
      (setf d4 (mod64+ d4 c)
            c (logand (mod64ash d4 -26) #xffffffff)
            h4 (logand d4 #x3ffffff))
      (setf h0 (mod32+ h0 (mod32* c 5))
            c (mod32ash h0 -26)
            h0 (logand h0 #x3ffffff))
      (setf h1 (mod32+ h1 c))
      (incf start 16)
      (decf remaining 16))
    (setf (aref accumulator 0) h0
          (aref accumulator 1) h1
          (aref accumulator 2) h2
          (aref accumulator 3) h3
          (aref accumulator 4) h4)
    (values start remaining)))

(defun update-poly1305 (mac data &key (start 0) (end (length data)))
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end)
           (optimize (speed 3) (space 0) (safety 1) (debug 0)))
  (let ((buffer (poly1305-buffer mac))
        (buffer-length (poly1305-buffer-length mac))
        (accumulator (poly1305-accumulator mac))
        (r (poly1305-r mac))
        (remaining (- end start)))
    (declare (type (simple-array (unsigned-byte 8) (16)) buffer)
             (type (integer 0 16) buffer-length)
             (type (simple-array (unsigned-byte 32) (5)) accumulator r)
             (type fixnum remaining))

    ;; Fill the buffer with new data if necessary
    (when (plusp buffer-length)
      (let ((n (min remaining (- 16 buffer-length))))
        (declare (type (integer 0 16) n))
        (replace buffer data
                 :start1 buffer-length
                 :start2 start
                 :end2 (+ start n))
        (incf buffer-length n)
        (incf start n)
        (decf remaining n)))

    ;; Process the buffer
    (when (= buffer-length 16)
      (poly1305-process-full-blocks accumulator r buffer 0 16 nil)
      (setf buffer-length 0))

    ;; Process the data
    (multiple-value-setq (start remaining)
      (poly1305-process-full-blocks accumulator r data start remaining nil))

    ;; Put the remaining data in the buffer
    (when (plusp remaining)
      (replace buffer data :start1 0 :start2 start :end2 end)
      (setf buffer-length remaining))

    ;; Save the state
    (setf (poly1305-buffer-length mac) buffer-length)
    (values)))

(defun poly1305-digest (mac)
  (let ((buffer (copy-seq (poly1305-buffer mac)))
        (buffer-length (poly1305-buffer-length mac))
        (accumulator (copy-seq (poly1305-accumulator mac)))
        (r (copy-seq (poly1305-r mac)))
        (s (copy-seq (poly1305-s mac))))
    (declare (type (simple-array (unsigned-byte 8) (16)) buffer)
             (type (integer 0 16) buffer-length)
             (type (simple-array (unsigned-byte 32) (5)) accumulator r)
             (type (simple-array (unsigned-byte 32) (4)) s))

    ;; Process the buffer
    (when (plusp buffer-length)
      (setf (aref buffer buffer-length) 1)
      (when (< buffer-length 15)
        (fill buffer 0 :start (1+ buffer-length) :end 16))
      (poly1305-process-full-blocks accumulator r buffer 0 16 t))

    ;; Produce the tag
    (let ((h0 (aref accumulator 0))
          (h1 (aref accumulator 1))
          (h2 (aref accumulator 2))
          (h3 (aref accumulator 3))
          (h4 (aref accumulator 4))
          (c 0)
          (g0 0)
          (g1 0)
          (g2 0)
          (g3 0)
          (g4 0)
          (f 0)
          (mask 0))
      (declare (type (unsigned-byte 32) h0 h1 h2 h3 h4 c g0 g1 g2 g3 g4 mask)
               (type (unsigned-byte 64) f))
      (setf c (mod32ash h1 -26)
            h1 (logand h1 #x3ffffff))
      (setf h2 (mod32+ h2 c)
            c (mod32ash h2 -26)
            h2 (logand h2 #x3ffffff))
      (setf h3 (mod32+ h3 c)
            c (mod32ash h3 -26)
            h3 (logand h3 #x3ffffff))
      (setf h4 (mod32+ h4 c)
            c (mod32ash h4 -26)
            h4 (logand h4 #x3ffffff))
      (setf h0 (mod32+ h0 (mod32* c 5))
            c (mod32ash h0 -26)
            h0 (logand h0 #x3ffffff))
      (setf h1 (mod32+ h1 c))

      (setf g0 (mod32+ h0 5)
            c (mod32ash g0 -26)
            g0 (logand g0 #x3ffffff))
      (setf g1 (mod32+ h1 c)
            c (mod32ash g1 -26)
            g1 (logand g1 #x3ffffff))
      (setf g2 (mod32+ h2 c)
            c (mod32ash g2 -26)
            g2 (logand g2 #x3ffffff))
      (setf g3 (mod32+ h3 c)
            c (mod32ash g3 -26)
            g3 (logand g3 #x3ffffff))
      (setf g4 (mod32- (mod32+ h4 c) #.(ash 1 26)))

      (setf mask (mod32- (mod32ash g4 -31) 1)
            g0 (logand g0 mask)
            g1 (logand g1 mask)
            g2 (logand g2 mask)
            g3 (logand g3 mask)
            g4 (logand g4 mask)
            mask (mod32lognot mask)
            h0 (logior (logand h0 mask) g0)
            h1 (logior (logand h1 mask) g1)
            h2 (logior (logand h2 mask) g2)
            h3 (logior (logand h3 mask) g3)
            h4 (logior (logand h4 mask) g4))

      (setf h0 (logior h0 (mod32ash h1 26))
            h1 (logior (mod32ash h1 -6) (mod32ash h2 20))
            h2 (logior (mod32ash h2 -12) (mod32ash h3 14))
            h3 (logior (mod32ash h3 -18) (mod32ash h4 8)))

      (setf f (mod64+ h0 (aref s 0))
            h0 (logand f #xffffffff))
      (setf f (mod64+ h1 (mod64+ (aref s 1) (mod64ash f -32)))
            h1 (logand f #xffffffff))
      (setf f (mod64+ h2 (mod64+ (aref s 2) (mod64ash f -32)))
            h2 (logand f #xffffffff))
      (setf f (mod64+ h3 (mod64+ (aref s 3) (mod64ash f -32)))
            h3 (logand f #xffffffff))

      (let ((tag (make-array 16 :element-type '(unsigned-byte 8))))
        (setf (ub32ref/le tag 0) h0
              (ub32ref/le tag 4) h1
              (ub32ref/le tag 8) h2
              (ub32ref/le tag 12) h3)
        tag))))

(defmac poly1305
        make-poly1305
        update-poly1305
        poly1305-digest)
