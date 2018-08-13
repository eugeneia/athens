(in-package :percent)

(define-condition decode-error (error)
  ((position :reader decode-error-position :initarg :position)))

(define-condition invalid-% (decode-error)
  ()
  (:report (lambda (c s)
             (format s "Encountered an invalid % at ~d."
                     (decode-error-position c)))))

(define-condition invalid-hexdig (decode-error)
  ((c1 :reader invalid-hexdig-c1 :initarg :c1)
   (c2 :reader invalid-hexdig-c2 :initarg :c2))
  (:report (lambda (c s)
             (format s "Encountered an invalid octet %~c~c at ~d."
                     (invalid-hexdig-c1 c)
                     (invalid-hexdig-c2 c)
                     (decode-error-position c)))))
