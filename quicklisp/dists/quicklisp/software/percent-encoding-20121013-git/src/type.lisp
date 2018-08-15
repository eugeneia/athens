(in-package :percent)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional size)
  `(simple-array octet (,size)))
