(in-package :percent)

(defvar *ascii-char-table*
  #(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\?
    #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O
    #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_
    #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
    #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~ nil))

(declaim (inline ascii-char))
(defun ascii-char (x)
  (aref (the (simple-array (or character null)) *ascii-char-table*) x))

(defvar *char-ascii-table*
  (labels ((->code (x)
             (etypecase x
               (character (char-code x))
               (integer x)
               (null 0)))
           (max-code (x y)
             (let ((x (->code x)) (y (->code y)))
               (max x y))))
    (let* ((length (1+ (reduce #'max-code *ascii-char-table*)))
           (table (make-array length :initial-element nil)))
      (reduce (lambda (i x)
                (when x (setf (aref table (char-code x)) i))
                (1+ i))
              *ascii-char-table* :initial-value 0)
      table)))

(declaim (inline char-ascii))
(defun char-ascii (x)
  (aref (the (simple-array (or octet null)) *char-ascii-table*) (char-code x)))
