;;;; TRIVIAL-FEED.XML: XML processing functions.

(in-package :trivial-feed.xml)

;; XMLS has no concept of character encodings so we need to apply heuristics:
;; https://www.w3.org/TR/xml/#charencoding

(defparameter *pi-start* (string-to-octets "<?"  :external-format :ascii))
(defparameter *pi-end*   (string-to-octets "?>"  :external-format :ascii))
(defparameter *xml*      (string-to-octets "xml" :external-format :ascii))

(defparameter *max-decl-size* 200
  "XML declarations exceeding *MAX-DECL-SIZE* bytes are ignored.")

(eval-when (:compile-toplevel :load-toplevel)
  (let ((s "[\\s\\r]*")                 ; (#x20 | #x9 | #xD | #xA)+?
        (enc-name "[A-Za-z0-9\\._-]+")) ; [A-Za-z] ([A-Za-z0-9._] | '-')*
    (defparameter *encoding-scanner*
      ;; S 'encoding' Eq ('"' EncName '"' | "'" EncName "'")
      (create-scanner (format nil "encoding~a=~a(\"(~a)\"|'(~a)')"
                              s s enc-name enc-name)))))

(defun xml-encoding-decl (string)
  (register-groups-bind (() a b) (*encoding-scanner* string)
    (external-format-name (make-external-format (or a b)))))

(defun xml-encoding (in)
  (let ((buffer (make-array *max-decl-size* :element-type '(unsigned-byte 8)))
        (pos 0))
    (labels ((fill-buffer (n)
               (setf pos (read-sequence buffer in :start pos :end (+ pos n))))
             (buffer-to-ascii ()
               (octets-to-string buffer :end pos :external-format :ascii))
             (return-encoding (&optional (encoding :utf-8)
                                         (read (buffer-to-ascii)))
               (return-from xml-encoding (values encoding read))))
      (fill-buffer 2)
      (when (search #(#xFF #xFE) buffer :end2 pos)
        (return-encoding :utf-16 ""))
      (unless (search *pi-start* buffer :end2 pos)
        (return-encoding))
      (fill-buffer 3)
      (unless (search *xml* buffer :start2 2 :end2 pos)
        (return-encoding))
      (loop when (= pos (fill-buffer 1)) do (return-encoding)
         until (search *pi-end* buffer :start2 (- pos 2) :end2 pos))
      (let ((read (buffer-to-ascii)))
        (return-encoding (xml-encoding-decl read) read)))))

(defun make-xml-stream (octet-stream)
  (multiple-value-bind (encoding read) (xml-encoding octet-stream)
    (make-concatenated-stream (make-string-input-stream read)
                              (make-flexi-stream octet-stream
                                                 :external-format encoding
                                                 :element-type 'character))))

(defun node-by-name (name)
  (lambda (node)
    (when (stringp #1=(node-name node))
      (string-equal name #1#))))

(defun node-text (node)
  (with-output-to-string (out)
    (loop for child in (node-children node)
       do (write-string (etypecase child
                          (string child)
                          (cons (toxml child)))
                        out))))

(defun attribute (string attributes)
  (second (assoc string attributes :test #'equalp)))
