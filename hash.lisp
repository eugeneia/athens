;;;; ATHENS.HASH: Hashing routines for feeds and feed items.

(in-package :athens.hash)

(defun feed-item-hash (feed-item)
  (digest-sequence
   :sha1
   (destructuring-bind (&key link date author title description language)
       feed-item
     (declare (ignore language date))
     (string-to-utf-8-bytes
      (format nil "~S~S~S~S" link author title description)))))

(defun feed-hash (feed)
  (digest-sequence
   :sha1
   (string-to-utf-8-bytes (getf feed :link))))
