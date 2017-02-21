;;;; TRIVIAL-FEED: Parse syndication feeds such as RSS and ATOM to a
;;;; canoical form and provide a hashing method for feed items.

(in-package :trivial-feed)

(defun decoded-input (s)
  (cond ((typep s '(array (unsigned-byte 8)))
         (with-input-from-sequence (in s)
           (decoded-input in)))
        ((and (streamp s)
              (subtypep (stream-element-type s) '(unsigned-byte 8)))
         (make-xml-stream s))))

(defun parse-feed (s &optional (fallback-date (get-universal-time)))
  (let ((*date* fallback-date)
        (feed-tree (parse (or (decoded-input s) s) :compress-whitespace t)))
    (cond ((rss-feed-p feed-tree) (parse-rss feed-tree))
          ((atom-feed-p feed-tree) (parse-atom feed-tree))
          ;; Unsupported format, fail.
          (t (error "Failed to find parser for feed from STREAM.")))))
