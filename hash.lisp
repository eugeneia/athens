;;;; ATHENS.HASH: Hashing routines for feeds and feed items.

(in-package :athens.hash)

(defun feed-item-hash (feed-item)
  (byte-array-to-hex-string
   (digest-sequence
    :sha1
    (destructuring-bind
          (&key link date author title description language)
        feed-item
      (declare (ignore language date))
      (string-to-octets
       (format nil "~S~S~S~S" link author title description)
       :external-format :utf-8)))))
