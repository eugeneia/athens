(in-package :percent)

(defun encode (string &key (test #'unreservedp)
                           (www-form nil)
                           (encoding *default-character-encoding*))
  (declare #.*optimize-qualities*
           (type (function (octet) boolean) test))
  (do* ((octets (string-to-octets string :encoding encoding))
        (length (length octets))
        (buffer (make-string (* length 3)))
        (i 0 (1+ i))
        (j 0 (1+ j)))
       ((= i length) (shrink-vector buffer j))
    (declare (type octets octets) (type fixnum i j))
    (let ((octet (aref octets i)))
      (declare (type octet octet))
      (cond ((funcall test octet)
             (setf (aref buffer j) (ascii-char octet)))
            ((and www-form (= octet #x0d))  ; CR
             (setf (aref buffer j) #\return)
             (setf (aref buffer (incf j)) #\linefeed)
             (when (and (< i length) (= (aref octets (1+ i)) #x0a))
               (incf i)))
            ((and www-form (= octet #x0a))  ; LF
             (setf (aref buffer j) #\return)
             (setf (aref buffer (incf j)) #\linefeed))
            ((and www-form (= octet #x20))  ; SP
             (setf (aref buffer j) #\+))
            (t
             (multiple-value-bind (h l) (truncate octet #x10)
               (setf (aref buffer j) #\%)
               (setf (aref buffer (incf j)) (digit-char h 16))
               (setf (aref buffer (incf j)) (digit-char l 16))))))))
