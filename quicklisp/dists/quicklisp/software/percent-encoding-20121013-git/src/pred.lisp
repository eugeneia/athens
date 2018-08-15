(in-package :percent)

(declaim (inline gen-delims-p))
(defun gen-delims-p (x)
  (case x
    ;;  :    /    ?    #    [    ]    @
    ((#x3a #x2f #x3f #x23 #x5b #x5d #x40) t)
    (t nil)))

(declaim (inline sub-delims-p))
(defun sub-delims-p (x)
  (case x
    ;;  !    $    &    '    (    )    *    +    ,    ;    =
    ((#x21 #x24 #x26 #x27 #x28 #x29 #x2a #x2b #x2c #x3b #x3d) t)
    (t nil)))

(declaim (inline reservedp))
(defun reservedp (x)
  (or (gen-delims-p x) (sub-delims-p x)))

(declaim (inline alphap))
(defun alphap (x)
  (or (<= #x41 x #x5a)      ; A-Z
      (<= #x61 x #x7a)))    ; a-z

(declaim (inline digitp))
(defun digitp (x)
  (<= #x30 x #x39)) ; 0-9

(declaim (inline unreservedp))
(defun unreservedp (x)
  (or (alphap x)
      (digitp x)
      (case x
        ;;  -    .    _    ~
        ((#x2d #x2e #x5f #x7e) t)
        (t nil))))

(declaim (inline userinfop))
(defun userinfop (x)
  (or (unreservedp x) (sub-delims-p x) (= x #x3a)))   ; :

(declaim (inline reg-name-p))
(defun reg-name-p (x)
  (or (unreservedp x) (sub-delims-p x)))

(declaim (inline pcharp))
(defun pcharp (x)
  (or (unreservedp x)
      (sub-delims-p x)
      (case x
        ;;  :    @
        ((#x3a #x40) t)
        (t nil))))

(declaim (inline queryp))
(defun queryp (x)
  (or (pcharp x)
      (case x
        ;;  /    ?
        ((#x2f #x3f) t)
        (nil t))))

(declaim (inline fragmentp))
(defun fragmentp (x)
  (queryp x))
