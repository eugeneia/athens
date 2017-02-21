;;;; ATHENS.HASH: Hashing routines for feeds and feed items.

(in-package :athens.hash)

(defun url-hash (url)
  "Return SHA-1 hash string for URL."
  (byte-array-to-hex-string
   (digest-sequence
    :sha1
    (string-to-octets url :external-format :utf-8))))
