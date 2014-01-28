;;;; ATHENS.HASH: Hashing routines for feeds and feed items.

(in-package :athens.hash)

(defun url-hash (url)
  "Return SHA-1 hash string for URL."
  (byte-array-to-hex-string
   (digest-sequence
    :sha1
    (string-to-octets url :external-format :utf-8))))

(defun feed-item-hash (feed-item)
  "Return SHA-1 hash string for FEED-ITEM."
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
