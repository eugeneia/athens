;;;; HTML widgets for Athens.

(in-package :athens.widgets)

(in-readtable macro-html:syntax)

(defparameter *scripts* '("/script"))

(defun html-widget-frontend ()
  "HTML widget for frontend application (Magnifier)."
  (html (html-widget-head "Athens Magnifier" :scripts *scripts*)
        (body)))

(defun feed-path (hash)
  "Make path to feed by HASH."
  (format nil "/feed/~a.html" hash))

(defun item-path (hash)
  "Make path to item by HASH."
  (format nil "/item/~a.html" hash))

(defun html-widget-footer ()
  "HTML widget for Athens footer."
  (hr)
  (em "Powered by Athens."))

(defun truncate-string (string &optional (length 32))
  "Truncate STRING to LENGTH."
  (if (> (length string) length)
      (format nil "~a…" (subseq string 0 (1- length)))
      string))

(defun date-string (universal-time &key (include-timezone t))
  (with-output-to-string (out)
    (let ((date-string (universal-time-to-http-date universal-time)))
      (write-string date-string out :start 0 :end (- (length date-string) 7)))
    (when include-timezone
      (write-string " GMT" out))))

(defun html-widget-feed (feed)
  "HTML widget for FEED."
  (destructuring-bind (&key source link date title description)
      feed
    (html-widget-document
     (or title "Untitled")
     (lambda ()
       (when title
         (header (b title)))
       (aside
        (dl
         (dt "Source")
         (dd (a [:href source] (truncate-string source)))
         (dt "Last updated")
         (dd (date-string date))
         (when link
           (dt "Link")
           (dd (a [:href link] (truncate-string link))))))
       (when description
         (article (progn (write-string description)
                         (values))))
       (html-widget-footer)))))

(defun html-widget-item (item)
  "HTML widget for ITEM."
  (destructuring-bind (&key feed link date author title description language)
      item
    (html-widget-document
     (or title "Untitled")
     (lambda ()
       (when title
         (header (b title)))
       (aside
        (dl
         (when author
           (dt "Author")
           (dd author))
         (dt "Date")
         (dd (date-string date))
         (when link
           (dt "Link")
           (dd (a [:href link] (truncate-string link))))
         (dt "Feed")
         (dd (a [:href (feed-path feed)] (truncate-string feed)))
         (when language
           (dt "Language")
           (dd language))))
       (when description
         (article (progn (write-string description)
                         (values))))
       (html-widget-footer)))))

(defun html-widget-news (news)
  "HTML widget for NEWS."
  (destructuring-bind (&key start end items) news
    (let ((title (format nil "News between ~a and ~a"
                         (date-string start)
                         (date-string end))))
      (html-widget-document
       title
       (lambda ()
         (header (b title))
         (if items
             (loop for (hash item) in items
                do (p (date-string (getf item :date) :include-timezone nil) "—"
                      (a [:href (item-path hash)]
                         (getf item :title))))
             (p "No news during that time."))
         (html-widget-footer))))))
