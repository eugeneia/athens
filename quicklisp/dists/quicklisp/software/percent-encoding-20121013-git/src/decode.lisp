(in-package :percent)

(defun decode (string &key (test (load-time-value (constantly t)))
                           (www-form nil)
                           (encoding *default-character-encoding*))
  (declare #.*optimize-qualities*
           (type simple-string string)
           (type (function (octet) boolean) test))
  (labels ((->octet (c1 c2 pos)
             (aif (char-pair-octet c1 c2)
                  it
                  (error 'invalid-hexdig :position pos :c1 c1 :c2 c2))))
    (do* ((length (length string))
          (buffer (make-array length :element-type 'octet))
          (i 0 (1+ i))
          (j 0 (1+ j)))
         ((= i length) (octets-to-string buffer :end j :encoding encoding))
      (declare (type fixnum i j))
      (let ((c (char string i)))
        (cond ((char/= c #\%)
               (if (and www-form (char= c #\+))
                   (setf (aref buffer j) #x20)  ; SP
                   (setf (aref buffer j) (char-ascii c))))
              ((> (+ i 3) length)
               (error 'invalid-% :position i))
              (t
               (let* ((c1 (char string (+ i 1)))
                      (c2 (char string (+ i 2)))
                      (octet (->octet c1 c2 i)))
                 (incf i 2)
                 (cond ((funcall test octet)
                        (setf (aref buffer j) octet))
                       (t
                        (setf (aref buffer j) #x25) ; '%'
                        (setf (aref buffer (incf j)) (char-ascii c1))
                        (setf (aref buffer (incf j)) (char-ascii c2)))))))))))
