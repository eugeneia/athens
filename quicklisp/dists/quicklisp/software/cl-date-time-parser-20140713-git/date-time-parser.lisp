;;;; Last modified: 2014-05-18 14:40:06 tkych

;; cl-date-time-parser/date-time-parser.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-date-time-parser/LICENSE


;;====================================================================
;; Date-Time-Parser
;;====================================================================

(in-package :cl-user)
(defpackage #:cl-date-time-parser
  (:use :cl)
  (:nicknames #:date-time-parser)
  (:import-from #:anaphora       #:it #:aif #:acond)
  (:import-from #:split-sequence #:split-sequence)
  (:import-from #:parse-float    #:parse-float)
  (:export #:parse-date-time))

(in-package #:cl-date-time-parser)


;;--------------------------------------------------------------------
;; Eval-Test for CL-Date-Time-Parser
;;--------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *features-tmp* *features*)

  ;; when release, the following line should be comment in.
  (setf *features* (remove :et *features*))

  ;; when release, the following two lines should be comment out.
  ;; (ql:quickload '(:cl-ppcre :split-sequence :anaphora :local-time :parse-float))
  ;; (pushnew :et *features*)
  )

#+et
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro =>? (form want &optional test)
    "Check whether FORM is evaluated to WANT by TEST (default is `equal`).
If first element of WANT is `:values`, then check mutiple values."
    (if (and (listp want) (eq :values (first want)))
        `(assert (funcall ,(if test test ''equal)
                          (multiple-value-list ,form)
                          (list ,@(rest want))))
        `(assert (funcall ,(if test test ''equal)
                          ,form ,want))))

  (defmacro =>t? (form)
    "Check whether FORM is evaluated to T."
    `(=>? ,form t))

  (defmacro =>nil? (form)
    "Check whether FORM is evaluated to NIL."
    `(=>? ,form nil))

  ) ;end of #+et


;;--------------------------------------------------------------------
;; Special Variables
;;--------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +second+       1)
  (defconstant +minuite-secs+ (* 60 +second+))
  (defconstant +hour-secs+    (* 60 +minuite-secs+))
  (defconstant +day-secs+     (* 24 +hour-secs+)))

(defparameter *month-vec-in-normal-year*
  #("0 is not month number."
    0 2678400 5097600 7776000 10368000 13046400 15638400 18316800
    20995200 23587200 26265600 28857600))

(defparameter *month-vec-in-leap-year*
  #("0 is not month number."
    0 2678400 5184000 7862400 10454400 13132800 15724800 18403200
    21081600 23673600 26352000 28944000))

(defparameter *month-ht-in-normal-year*
  (alexandria:plist-hash-table
   '("Jan" 0
     "Feb" 2678400
     "Mar" 5097600
     "Apr" 7776000
     "May" 10368000
     "Jun" 13046400
     "Jul" 15638400
     "Aug" 18316800
     "Sep" 20995200
     "Oct" 23587200
     "Nov" 26265600
     "Dec" 28857600

     ;; invalid month name
     "January"   0
     "February"  2678400
     "March"     5097600
     "April"     7776000
     "May"       10368000
     "June"      13046400
     "July"      15638400
     "August"    18316800
     "September" 20995200
     "October"   23587200
     "November"  26265600
     "December"  28857600)
   :test #'equalp))

(defparameter *month-ht-in-leap-year*
  (alexandria:plist-hash-table
   '("Jan" 0
     "Feb" 2678400
     "Mar" 5184000
     "Apr" 7862400
     "May" 10454400
     "Jun" 13132800
     "Jul" 15724800
     "Aug" 18403200
     "Sep" 21081600
     "Oct" 23673600
     "Nov" 26352000
     "Dec" 28944000

     ;; invalid month name
     "January"   0
     "February"  2678400
     "March"     5184000
     "April"     7862400
     "May"       10454400
     "June"      13132800
     "July"      15724800
     "August"    18403200
     "September" 21081600
     "October"   23673600
     "November"  26352000
     "December"  28944000)
   :test #'equalp))

(defparameter *tz-abbrev-to-offest*
  (alexandria:plist-hash-table
   '("UT"  0
     "GMT" 0
     "ADT" -10800
     "AST" -14400
     "EDT" -14400
     "EST" -18000
     "CDT" -18000
     "CST" -21600
     "MDT" -21600
     "MST" -25200
     "PDT" -25200
     "PST" -28800

     "AT" -14400
     "ET" -18000
     "CT" -21600
     "MT" -25200
     "PT" -28800

     ;; Memo: Military-time-zone is defined by rfc822, and obsoluted by rfc1123,
     ;; rfc2822 and rfc5322. For more details see. rfc1123, 5.2.14.
     "A" 0 "B" 0 "C" 0 "D" 0 "E" 0 "F" 0 "G" 0 "H" 0 "I" 0
     "K" 0 "L" 0 "M" 0 "N" 0 "O" 0 "P" 0 "Q" 0 "R" 0 "S" 0
     "T" 0 "U" 0 "V" 0 "W" 0 "X" 0 "Y" 0
     ;; Zulu time
     "Z" 0
     )
   :test #'equalp))

(defparameter *day-of-week*
  (alexandria:plist-hash-table
   '("Mon" 1
     "Tue" 1
     "Wed" 1
     "Thu" 1
     "Fri" 1
     "Sat" 1
     "Sun" 1

     ;; invalid day name
     "Monday"    1
     "Tuesday"   1
     "Wednesday" 1
     "Thursday"  1
     "Friday"    1
     "Saturday"  1
     "Sunday"    1)
   :test #'equalp))


;;--------------------------------------------------------------------
;; Parse-Date-Time
;;--------------------------------------------------------------------

(defun get-offset (tz-abbrev)
  (aif (gethash tz-abbrev *tz-abbrev-to-offest* nil)
       it
       (let ((offset (handler-case
                         (calc-offset tz-abbrev)
                       (error ()
                         (error "~S is not parsed as time-zone."
                                tz-abbrev)))))
         (setf (gethash tz-abbrev *tz-abbrev-to-offest*)
               offset))))

;; !!! UGLY: using un-official api for local-time. !!!
(defun calc-offset (tz-abbrev)
  "Return offset for the time-zone-abbrev. If not find offset, return NIL."
  (symbol-macrolet ((timezones local-time::*abbreviated-subzone-name->timezone-list*))
    (let ((tz (aif (gethash tz-abbrev timezones nil)
                   (first it)
                   (when (zerop (hash-table-count timezones))
                     (local-time::reread-timezone-repository)
                     (first (gethash tz-abbrev timezones nil))))))
      (when tz
        (loop
           :for sub :across (local-time::timezone-subzones tz)
           :do (when (equal tz-abbrev (local-time::subzone-abbrev sub))
                 (return (- (local-time::subzone-offset sub)))))))))

#+et (=>? (get-offset "JST") (* -9 60 60))
#+et (=>? (get-offset "GMT") 0)
#+et (=>? (get-offset "MDT") (* -6 60 60))


(defun year-to-ut (year)
  (encode-universal-time 0 0 0 1 1 year 0))

#+et (=>? (year-to-ut 1900) 0)
#+et (=>? (year-to-ut 1901) (* 365 24 60 60))
#+et (=>? (year-to-ut 1902) (+ (year-to-ut 1901) (* 365 24 60 60)))
#+et (=>? (year-to-ut 1905) (+ (year-to-ut 1904) (* 366 24 60 60)))
#+et (=>? (year-to-ut 2000)
          (local-time:timestamp-to-universal
           (local-time:parse-timestring "2000-01-01T00:00:00Z")))
#+et (=>? (year-to-ut 2013)
          (local-time:timestamp-to-universal
           (local-time:parse-timestring "2013-01-01T00:00:00Z")))


(defun leap-year-p (year)
  "Return T if YEAR is a leap year, otherwise NIL.
c.f. RFC3339, (Appendix C. Leap Years)"
  (check-type year (integer 1000 9999))
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

#+et (=>nil? (leap-year-p 1999))
#+et (=>t?   (leap-year-p 2000))
#+et (=>nil? (leap-year-p 2001))


(defun month-to-ut (month leap-year?)
  (etypecase month
    (string (gethash month (if leap-year?
                               *month-ht-in-leap-year*
                               *month-ht-in-normal-year*)))
    (integer (svref (if leap-year?
                        *month-vec-in-leap-year*
                        *month-vec-in-normal-year*)
                    month))))

#+et (=>t? (and (every (lambda (m) (= (month-to-ut m t)
                                      (month-to-ut m nil)))
                       '("Jan" "January" 1))
                (every (lambda (m) (= (month-to-ut m t)
                                      (month-to-ut m nil)))
                       '("Feb" "February" 2))
                (every (lambda (m) (= (* 24 60 60)
                                      (- (month-to-ut m t)
                                         (month-to-ut m nil))))
                       '("Mar" "March" 3))
                (every (lambda (m) (= (* 24 60 60)
                                      (- (month-to-ut m t)
                                         (month-to-ut m nil))))
                       '("Apr" "April" 4))
                (every (lambda (m) (= (* 24 60 60)
                                      (- (month-to-ut m t)
                                         (month-to-ut m nil))))
                       '("Dec" "December" 12))))


(defun parse-rfc822-genus (date-time-string)
  "Parse DATE-TIME-STRING with RFC822 (RFC1123, RFC2822, RFC5322),
RFC850 (RFC1036) or asctime format, and return
 (values UNIVERSAL-TIME FRACTION).

Reference:
 * RFC822  -- http://tools.ietf.org/html/rfc822
 * RFC2822 -- http://tools.ietf.org/html/rfc2822
 * RFC5322 -- http://tools.ietf.org/html/rfc5322
 * RFC850  -- http://tools.ietf.org/html/rfc850
 * RFC1036 -- http://tools.ietf.org/html/rfc1036
 * asctime -- http://en.cppreference.com/w/c/chrono/asctime
"
  (let ((universal-time 0)
        (fraction 0)
        (leap-year? 0)
        (month nil)
        (day-parsed? nil))
    (flet ((parse-time-part (token)
             ;; hh:mm(:ss([:.]ss?)?)?
             (let ((time-parts (ppcre:split "[.:]" token)))
               (loop
                  :for d :in time-parts
                  :for secs :in '#.(list +hour-secs+ +minuite-secs+ +second+)
                  :do (incf universal-time (* secs (parse-integer d))))
               (when (= 4 (length time-parts))
                 (let ((frac-part (car (last time-parts))))
                   (setf fraction
                         (parse-float
                          (replace (copy-seq "0.0000")
                                   frac-part :start1 2)))))))

           (parse-year (year)
             (incf universal-time (year-to-ut year))
             (setf leap-year? (leap-year-p year)))

           (parse-days (token) ; "DD"
             (let ((num-days (parse-integer token)))
               (incf universal-time (* (1- num-days) +day-secs+)))))

      (dolist (token (ppcre:split "(?=[+-]\\d{2}:?\\d{2})|[, -]|(?=\\d[A-Za-z]+$)"
                                  date-time-string))
        (when (string/= "" token)
          ;; Memo:
          ;; * Check whether last char is digit-char or not,
          ;;   in order to interpret "+9000" and "4" as num-token
          ;;   !! TODO: check all time-zone-abbrev, whether not using digit-chars.
          ;; * alpha-char-p return T only when [a-zA-Z].
          ;;   Using digit-char-p, we enable to extend
          ;;   *month-secs-in-normal-year(or leap-year)* to non-alphabet local chars.
          (if (not (digit-char-p (char token (1- (length token)))))

              ;; A. Parse char-token
              (acond
                ;; Memo: consistency is not checking.
                ;; i.e. "Mon, 21 Jul 2013 07:22:21 GMT" and "Sun, 21 Jul 2013 07:22:21 GMT"
                ;;      are parsed to the same universal time value.
                ((gethash token *day-of-week* nil)
                 nil)
                ;; we don't know the year yet, calc month is after parse year.
                ((gethash token *month-ht-in-normal-year* nil)
                 (setf month token))
                ((get-offset token)
                 (incf universal-time it))
                (t nil))

              ;; B. Parse num-token
              (case (length token)
                ;; "DD", "YY"
                ((1 2) (if day-parsed?
                           (let* ((num (parse-integer token))
                                  ;; c.f. rfc5322, p.34 (4.3. Obsolete Date and Time)
                                  (year (cond ((<=  0 num 49) (+ 2000 num))
                                              ((<= 50 num 99) (+ 1900 num))
                                              (t num))))
                             (parse-year year))
                           (progn
                             (parse-days token)
                             (setf day-parsed? t))))
                ;; "YYY",  c.f. rfc5322, p.34 (4.3. Obsolete Date and Time)
                (3 (parse-year (+ 1900 (parse-integer token))))
                ;; "YYYY"
                (4 (parse-year (parse-integer token)))
                ;; hh:mm(:ss)?([+-]hh:?mm)? or [+-]hh:?mm
                (t (let ((tokens (ppcre:split "[+-]" token)))
                     (ecase (length tokens)
                       ;; "hh:mm:ss.ss", "hh:mm:ss", "hh:mm"
                       (1 (parse-time-part token))
                       ;; "hh:mm:ss+hh:mm", "hh:mm:ss+hhmm", "hh:mm+hh:mm", "hh:mm+hhmm"
                       (2 (destructuring-bind (time time-zone) tokens
                            (parse-time-part time)
                            (let ((sign (if (find #\+ token) -1 +1)) ;sign becomes inverse
                                  (hour (parse-integer time-zone :end 2))
                                  (minute (parse-integer
                                           time-zone :start (if (find #\: time-zone) 3 2))))
                              (incf universal-time (* sign (+ (* hour #.+hour-secs+)
                                                              (* minute #.+minuite-secs+))))))))))))))

      (when (equal 0 leap-year?)
        ;; Memo: get-decoded-time returns date-time depending system time-zone.
        (let ((this-year (nth-value 5 (decode-universal-time
                                       (get-universal-time) 0))))
          (warn "YEAR was not detected in ~S as RFC822-Genus. YEAR was supplemented with this year, \"~S\"."
                date-time-string this-year)
          (parse-year this-year)))

      (incf universal-time (month-to-ut month leap-year?)))

    (values universal-time fraction)))


#+et (flet ((rfc822 (x) (parse-rfc822-genus x))
            (enc (&rest args) (apply #'encode-universal-time args)))

       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 GMT")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 gmt")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 Z")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00Z")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 z")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00z")
            (:values (enc 0 0 0 1 1 2013 0) 0))

       (=>? (rfc822 "Sat, 02 Mar 2013 01:23:45 EDT")
            (:values (enc 45 23 1 2 3 2013 -4) 0))
       (=>? (rfc822 "Sat, 02 Mar 2013 01:23:45 FOOBAZBAR")
            (:values (enc 45 23 1 2 3 2013 0) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 JST")
            (:values (enc 0 0 0 1 1 2013 -9) 0))
       (=>? (rfc822 "Sat, 01 Feb 2013 01:23:45 JST")
            (:values (enc 45 23 1 1 2 2013 -9) 0))
       (=>? (rfc822 "01 Dec 13 00:00 JST")
            (:values (enc 0 0 0 1 12 2013 -9) 0))

       (=>? (rfc822 "24 Dec 49 12:00 EST")
            (:values (enc 0 0 12 24 12 2049 -5) 0))
       (=>? (rfc822 "24 Dec 50 12:00 EST")
            (:values (enc 0 0 12 24 12 1950 -5) 0))

       (=>? (rfc822 "Sat, 01 Jan 2000 00:00:00.42 GMT")
            (:values (enc 0 0 0 1 1 2000 0) 0.42))
       (=>? (rfc822 "Sat, 01 Jan 2000 00:00:00:42 GMT")
            (:values (enc 0 0 0 1 1 2000 0) 0.42))

       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 +0700")
            (:values (enc 0 0 0 1 1 2013 -7) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00+1300")
            (:values (enc 0 0 0 1 1 2013 -13) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00 +07:00")
            (:values (enc 0 0 0 1 1 2013 -7) 0))
       (=>? (rfc822 "Sat, 01 Jan 2013 00:00:00+13:00")
            (:values (enc 0 0 0 1 1 2013 -13) 0))
       (=>? (rfc822 "Sat, 01 Mar 2008 19:42:34 -0500")
            (:values (enc 34 42 19 1 3 2008 5) 0))
       (=>? (rfc822 "Sat, 01 Mar 2008 19:42:34-0500")
            (:values (enc 34 42 19 1 3 2008 5) 0))
       (=>? (rfc822 "Sat, 01 Mar 2008 19:42:34 -05:00")
            (:values (enc 34 42 19 1 3 2008 5) 0))
       (=>? (rfc822 "Sat, 01 Mar 2008 19:42:34-05:00")
            (:values (enc 34 42 19 1 3 2008 5) 0))

       (=>? (rfc822 "Thu, 01 Jan 2004")
            (:values (enc 0 0 0 1 1 2004 0) 0))
       (=>? (rfc822 "01 Jan 2004")
            (:values (enc 0 0 0 1 1 2004 0) 0))
       (=>? (rfc822 "1 Jan 04")
            (:values (enc 0 0 0 1 1 2004 0) 0))

       (=>? (rfc822 "Sun Jan 4 16:29:06 PST 2004")
            (:values (enc 6 29 16 4 1 2004 -8) 0))

       (=>? (rfc822 "Thu Jul 23 19:42:23 JST 2013")
            (:values (enc 23 42 19 23 7 2013 -9) 0))

       (=>? (rfc822 "Thudesday, 23-Jul-13 19:42:23 GMT")
            (:values (enc 23 42 19 23 7 2013 0) 0))

       ;; supplemental this year
       (let ((this-year (nth-value 5 (decode-universal-time
                                      (get-universal-time) 0))))
         (=>? (rfc822 "Thu, 01 Jan")
              (:values (enc 0 0 0 1 1 this-year 0) 0))
         (=>? (rfc822 "01 Jan")
              (:values (enc 0 0 0 1 1 this-year 0) 0))
         (=>? (rfc822 "1 Jan")
              (:values (enc 0 0 0 1 1 this-year 0) 0)))

       )


(defun parse-iso8601-genus (date-time-string)
  "Parse DATE-TIME-STRING with ISO8601, W3CDTF or RFC3339 format,
and return (values UNIVERSAL-TIME FRACTION).

Reference:
 * ISO8601:1988, 2000, 2004
           -- http://www.iso.org/iso/home/standards/iso8601.htm
 * W3CDTF  -- http://www.w3.org/TR/1998/NOTE-datetime-19980827
 * RFC3339 -- http://tools.ietf.org/html/rfc3339
"
  (let* ((universal-time 0)
         (fraction 0)
         (leap-year? nil)
         ;; date-time separater is #\T, #\t or #\Space, c.f. rfc3339, 5.6.
         (date-time (ppcre:split "(?<=\\d)[Tt ](?=\\d|$)" date-time-string))
         (date-part (first  date-time))
         (time-part (second date-time)))
    (when time-part
      (let* ((time-zone (ppcre:split "(?<=\\d)(?=[zZ+-])" time-part))
             (time-part (first  time-zone))
             (zone-part (second time-zone)))

        ;; 0. Parse ZONE-part:
        (when zone-part
          (unless (string-equal "Z" zone-part)
            (case (length zone-part)
              ((2 3)  ; "+h", "+hh"
               (decf universal-time (* #.+hour-secs+ (parse-integer zone-part))))
              (5      ; "+hhmm"
               (multiple-value-bind (h m)
                   (truncate (parse-integer zone-part) 100)
                 (decf universal-time (+ (* h #.+hour-secs+) (* m #.+minuite-secs+)))))
              (6      ; "+hh:mm"
               (multiple-value-bind (h m)
                   (truncate (parse-integer (remove #\: zone-part)) 100)
                 (decf universal-time (+ (* h #.+hour-secs+) (* m #.+minuite-secs+)))))
              (t
               (error "~S in ~S is unknown time-format as ISO8601-Genus"
                      zone-part date-time-string)))))

        ;; 1. Parse TIME-part:
        (if (every #'digit-char-p time-part)
            (loop  ;Basic format: "hh", "hhmm", "hhmmss", "hhmmssss"
               :repeat (ceiling (length time-part) 2)
               :for (start end) :in '((0 2) (2 4) (4 6))
               :for num := (parse-integer time-part :start start :end end)
               :for secs :in '#.(list +hour-secs+ +minuite-secs+ +second+)
               :do (incf universal-time (* num secs))
               :finally (when (<= 7 (length time-part))
                          (setf fraction
                                (parse-float (replace time-part "00000.")))))
            (loop  ;Extended format: "hh:mm", "hh:mm:ss", "hh:mm:ss,ss"
               :for d :in (ppcre:split "[:,.]" time-part) ;"," for iso8601, "." for rfc3339
               :for secs :in '#.(list +hour-secs+ +minuite-secs+ +second+)
               :do (incf universal-time (* (parse-integer d) secs))
               :finally (when (<= 10 (length time-part))
                          (setf fraction
                                (parse-float (replace time-part "00000000."))))))))

    ;; 2. Parse DATE-part:
    (labels
        ;; 2.0. Parse DATE-part (local functions):
        ((parse-weeks (token) ;"Www"
           (let ((num-weeks (parse-integer token :start 1 :end 3)))
             (incf universal-time (* 7 (1- num-weeks) +day-secs+))))

         (parse-days (token)  ;"D", "DDD"
           (let ((num-days (parse-integer token)))
             (incf universal-time (* (1- num-days) +day-secs+))))

         (parse-month (month leap-year?)
           (incf universal-time (month-to-ut month leap-year?)))

         (parse-year (year)
           (incf universal-time (year-to-ut year))
           (setf leap-year? (leap-year-p year)))

         (parse-extended-format (date)
           ;; Parse iso8601 extended format
           ;; "YYYY-MM", "YY-MM-DD", "YYYY-MM-DD", "YYYY-DDD", "YYYY-Www-D", "YYYYYY-DDD"
           (loop
              :for token :in (split-sequence #\- date)
              :with year-parsed?  := nil
              :with month-parsed? := nil
              :do (case (length token)
                    ;; "YY", "MM", "DD"
                    (2 (if year-parsed?
                           (if month-parsed?
                               (parse-days token)
                               (progn
                                 (parse-month (parse-integer token) leap-year?)
                                 (setf month-parsed? t)))
                           (progn
                             ;; c.f. RFC 3339, 3. Two Digit Years, last item
                             (when (<= #.(char-code #\:) (char-code (char token 0)))
                               (let ((broken-two-digit-year (copy-seq token)))
                                 (setf (char token 0)
                                       (digit-char (- (char-code (char token 0)) #.(char-code #\:))))
                                 (warn "Broken two-digit year ~S was parsed as \"~S\". (c.f. RFC 3339, 3.)"
                                       broken-two-digit-year (+ 2000 (parse-integer token)))))
                             (parse-year (+ 2000 (parse-integer token)))
                             (setf year-parsed? t))))
                    ;; "YYYY", "YYYYYY"
                    ((4 6) (parse-year (parse-integer token))
                           (setf year-parsed? t))
                    ;; "Www", "D", "DDD"
                    ((1 3) (if (find #\W token :test #'char-equal)
                               (parse-weeks token)
                               (parse-days token)))
                    (t (error "~S in ~S is unknown time-format as ISO8601-Genus"
                              token date)))))

         (parse-basic-format (date)
           ;; Parse iso8601 basic format
           ;; "CC", "DDD", "YYYY", "YYDDD", "YYMMDD", "YYYYDDD", "YYYYMMDD"
           (case (length date)
             ;; Memo:
             ;; ISO8601-century format can not be parsed.
             ;; ISO8601-century is a 2-digit which slided forward 99 years comparing to ordinay-century.
             ;; e.g. 20 iso8601-century means the year between 2000 and 2099,
             ;;      whereas usually 20th century means the year between 1901 and 2000.
             ;; "CC"
             (2 (error "ISO8601 century ~S could not be parsed." date))
             ;; "DDD"
             (3 (parse-days date))
             ;; "YYYY"
             (4 (parse-year (parse-integer date)))
             ;; "YYDDD"
             (5 ;; c.f. RFC 3339, 3. Two Digit Years, last item
                (when (<= #.(char-code #\:) (char-code (char date 0)))
                  (let ((broken-two-digit-year (subseq date 0 2)))
                    (setf (char date 0)
                          (digit-char (- (char-code (char date 0)) #.(char-code #\:))))
                    (warn "Broken two-digit year ~S was parsed as \"~S\". (c.f. RFC 3339, 3.)"
                          broken-two-digit-year (+ 2000 (parse-integer date :start 0 :end 2)))))
                (parse-year (+ 2000 (parse-integer date :start 0 :end 2)))
                (parse-days (subseq date 2)))
             ;; "YYMMDD"
             (6 (parse-year  (+ 2000 (parse-integer date :start 0 :end 2)))
                (parse-month (parse-integer date :start 2 :end 4) leap-year?)
                (parse-days  (subseq date 4)))
             ;; "YYYYDDD" 
             (7 (parse-year (parse-integer date :start 0 :end 4))
                (parse-days (subseq date 4)))
             ;; "YYYYMMDD"
             (8 (parse-year  (parse-integer date :start 0 :end 4))
                (parse-month (parse-integer date :start 4 :end 6) leap-year?)
                (parse-days  (subseq date 6)))
             (t (error "~S in ~S is unknown time-format as ISO8601-Genus"
                       date date-time-string)))))

      ;; 2.1. Parse DATE-part (main):
      (if (find #\- date-part)
          ;; Extended format:
          ;; "-YY", "-YY-MM", "-YYMM", "-YY-MM-DD",
          ;; "YYYY-MM", "YYYY-MM-DD", "YYYY-DDD", "YYYY-Www-D", "YYYYYY-DDD"
          (if (char= #\- (char date-part 0))
              ;; "-YY", "-YY-MM", "-YYMM", "-YY-MM-DD" -> "20YY", "20YY-MM", "20YYMM", "20YY-MM-DD"
              (let ((replaced (ppcre:regex-replace "-" date-part "20")))
                (if (find #\- replaced)
                    ;; "20YY-MM", "20YY-MM-DD"
                    (parse-extended-format replaced)
                    ;; "20YY", "20YYMM" -> "20YY0101", "20YYMM01"
                    (parse-basic-format (replace (copy-seq "00000101") replaced))))
              ;; "YYYY-MM", "YYYY-MM-DD", "YYYY-DDD", "YYYY-Www-D", "YYYYYY-DDD"
              (parse-extended-format date-part))
          ;; Basic format:
          ;; "CC", "YYYY", "YYDDD", "YYMMDD", "YYYYMMDD", "YYYYWwwD"
          (parse-basic-format date-part)))

    ;; 3. Return values
    (values universal-time fraction)))


#+et (flet ((iso8601 (x) (parse-iso8601-genus x))
            (enc (&rest args) (apply #'encode-universal-time args)))

       (=>? (iso8601 "2000-01-01T00:00:00Z")
            (:values (enc 0 0 0 1 1 2000 0) 0))

       (=>? (iso8601 "2003-12-31T10:14:55.7-08:00")
            (:values (enc 55 14 10 31 12 2003 +8) 0.7))
       (=>? (iso8601 "2003-12-31T10:14:55.7-0800")
            (:values (enc 55 14 10 31 12 2003 +8) 0.7))

       (=>? (iso8601 "2003-12-31T10:14:55+08:00")
            (:values (enc 55 14 10 31 12 2003 -8) 0))
       (=>? (iso8601 "2003-12-31T10:14:55+0800")
            (:values (enc 55 14 10 31 12 2003 -8) 0))

       (=>? (iso8601 "2003-12-31T10:14:55.7Z")
            (:values (enc 55 14 10 31 12 2003 0) 0.7))
       (=>? (iso8601 "2003-12-31T10:14:55Z")
            (:values (enc 55 14 10 31 12 2003 0) 0))

       (=>? (iso8601 "2003-12-31T10:14Z")
            (:values (enc 0 14 10 31 12 2003 0) 0))

       (=>? (iso8601 "2003-12-31T101455-08:00")
            (:values (enc 55 14 10 31 12 2003 +8) 0))
       (=>? (iso8601 "2003-12-31T101455-0800")
            (:values (enc 55 14 10 31 12 2003 +8) 0))
       (=>? (iso8601 "2003-12-31T1014-08:00")
            (:values (enc 0 14 10 31 12 2003 +8) 0))
       (=>? (iso8601 "2003-12-31T1014-0800")
            (:values (enc 0 14 10 31 12 2003 +8) 0))
       (=>? (iso8601 "2003-12-31T1014557Z")
            (:values (enc 55 14 10 31 12 2003 0) 0.7))
       (=>? (iso8601 "2003-12-31T101455Z")
            (:values (enc 55 14 10 31 12 2003 0) 0))
       (=>? (iso8601 "2003-12-31T1014Z")
            (:values (enc 0 14 10 31 12 2003 0) 0))
       (=>? (iso8601 "2003-12-31T10Z")
            (:values (enc 0 0 10 31 12 2003 0) 0))

       (=>? (iso8601 "2003")
            (:values (enc 0 0 0 1 1 2003 0) 0))
       (=>? (iso8601 "2003-12")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (iso8601 "2003-12-31")
            (:values (enc 0 0 0 31 12 2003 0) 0))
       (=>? (iso8601 "20031231")
            (:values (enc 0 0 0 31 12 2003 0) 0))

       (=>? (iso8601 "-03-12")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (iso8601 "-03")
            (:values (enc 0 0 0 1 1 2003 0) 0))
       (=>? (iso8601 "-0312")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (iso8601 "-03-12-31")
            (:values (enc 0 0 0 31 12 2003 0) 0))

       (=>? (iso8601 "03-12-31")
            (:values (enc 0 0 0 31 12 2003 0) 0))

       (=>? (iso8601 "031231")
            (:values (enc 0 0 0 31 12 2003 0) 0))

       (=>? (iso8601 "2003335")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (iso8601 "2003-335")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (iso8601 "03335")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (iso8601 "03-335")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       )


(defun parse-date-time (date-time-string)
  "Parse DATE-TIME-STRING, and return (values UNIVERSAL-TIME FRACTION).
DATE-TIME-STRING must represent the date-time after 1900-01-01T00:00:00Z.

Parsable Formats:
 * RFC822 Genus: RFC822 (RFC1123, RFC2822, RFC5322), RFC850 (RFC1036) and asctime.
 * ISO8601 Genus: ISO 8601 (:1988, :2000 and :2004. except for no-year format),
                  W3CDTF, RFC3339.
 * Broken format: The above formats with little broken.

Examples:
 * (parse-date-time \"Thu, 23 Jul 2013 19:42:23 JST\")
   => 3583564943, 0

 * (parse-date-time \"2013-07-23T19:42:23+09:00\")
   => 3583564943, 0

 * (parse-date-time \"23 Jul 13 19:42:23 +0900\")
   => 3583564943, 0

 * (parse-date-time \"Thu Jul 23 19:42:23 JST 2013\")
   => 3583564943, 0

 * (parse-date-time \"2013-07-23T19:42:23.45Z\")
   => 3583597343, 0.45

 For more examples, see Eval-Test in date-time-parser.lisp"
  (check-type date-time-string string)
  (if (ppcre:scan "[a-zA-Z]{2}" date-time-string)
      (parse-rfc822-genus date-time-string)
      (parse-iso8601-genus date-time-string)))


#+et (flet ((parse (x) (parse-date-time x))
            (enc (&rest args) (apply #'encode-universal-time args)))

       ;; rfc822-genus
       (=>? (parse "Sat, 01 Jan 2013 00:00:00 GMT")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00 gmt")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00 Z")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00Z")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00 z")
            (:values (enc 0 0 0 1 1 2013 0) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00z")
            (:values (enc 0 0 0 1 1 2013 0) 0))

       (=>? (parse "Sat, 02 Mar 2013 01:23:45 EDT")
            (:values (enc 45 23 1 2 3 2013 -4) 0))
       (=>? (parse "Sat, 02 Mar 2013 01:23:45 FOOBAZBAR")
            (:values (enc 45 23 1 2 3 2013 0) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00 JST")
            (:values (enc 0 0 0 1 1 2013 -9) 0))
       (=>? (parse "Sat, 01 Feb 2013 01:23:45 JST")
            (:values (enc 45 23 1 1 2 2013 -9) 0))
       (=>? (parse "01 Dec 13 00:00 JST")
            (:values (enc 0 0 0 1 12 2013 -9) 0))

       (=>? (parse "24 Dec 49 12:00 EST")
            (:values (enc 0 0 12 24 12 2049 -5) 0))
       (=>? (parse "24 Dec 50 12:00 EST")
            (:values (enc 0 0 12 24 12 1950 -5) 0))

       (=>? (parse "Sat, 01 Jan 2000 00:00:00.42 GMT")
            (:values (enc 0 0 0 1 1 2000 0) 0.42))
       (=>? (parse "Sat, 01 Jan 2000 00:00:00:42 GMT")
            (:values (enc 0 0 0 1 1 2000 0) 0.42))

       (=>? (parse "Sat, 01 Jan 2013 00:00:00 +0700")
            (:values (enc 0 0 0 1 1 2013 -7) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00+1300")
            (:values (enc 0 0 0 1 1 2013 -13) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00 +07:00")
            (:values (enc 0 0 0 1 1 2013 -7) 0))
       (=>? (parse "Sat, 01 Jan 2013 00:00:00+13:00")
            (:values (enc 0 0 0 1 1 2013 -13) 0))

       (=>? (parse "Thu, 01 Jan 2004")
            (:values (enc 0 0 0 1 1 2004 0) 0))
       (=>? (parse "01 Jan 2004")
            (:values (enc 0 0 0 1 1 2004 0) 0))
       (=>? (parse "1 Jan 04")
            (:values (enc 0 0 0 1 1 2004 0) 0))

       (=>? (parse "Sun Jan 4 16:29:06 PST 2004")
            (:values (enc 6 29 16 4 1 2004 -8) 0))

       ;; iso8601-genus
       (=>? (parse "2000-01-01T00:00:00Z")
            (:values (enc 0 0 0 1 1 2000 0) 0))

       (=>? (parse "2003-12-31T10:14:55.7-08:00")
            (:values (enc 55 14 10 31 12 2003 +8) 0.7))
       (=>? (parse "2003-12-31T10:14:55.7-0800")
            (:values (enc 55 14 10 31 12 2003 +8) 0.7))

       (=>? (parse "2003-12-31T10:14:55+08:00")
            (:values (enc 55 14 10 31 12 2003 -8) 0))
       (=>? (parse "2003-12-31T10:14:55+0800")
            (:values (enc 55 14 10 31 12 2003 -8) 0))

       (=>? (parse "2003-12-31T10:14:55.7Z")
            (:values (enc 55 14 10 31 12 2003 0) 0.7))
       (=>? (parse "2003-12-31T10:14:55Z")
            (:values (enc 55 14 10 31 12 2003 0) 0))

       (=>? (parse "2003-12-31T10:14Z")
            (:values (enc 0 14 10 31 12 2003 0) 0))

       (=>? (parse "2003-12-31T101455-08:00")
            (:values (enc 55 14 10 31 12 2003 +8) 0))
       (=>? (parse "2003-12-31T101455-0800")
            (:values (enc 55 14 10 31 12 2003 +8) 0))
       (=>? (parse "2003-12-31T1014-08:00")
            (:values (enc 0 14 10 31 12 2003 +8) 0))
       (=>? (parse "2003-12-31T1014-0800")
            (:values (enc 0 14 10 31 12 2003 +8) 0))
       (=>? (parse "2003-12-31T1014557Z")
            (:values (enc 55 14 10 31 12 2003 0) 0.7))
       (=>? (parse "2003-12-31T101455Z")
            (:values (enc 55 14 10 31 12 2003 0) 0))
       (=>? (parse "2003-12-31T1014Z")
            (:values (enc 0 14 10 31 12 2003 0) 0))
       (=>? (parse "2003-12-31T10Z")
            (:values (enc 0 0 10 31 12 2003 0) 0))

       (=>? (parse "2003")
            (:values (enc 0 0 0 1 1 2003 0) 0))
       (=>? (parse "2003-12")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (parse "2003-12-31")
            (:values (enc 0 0 0 31 12 2003 0) 0))
       (=>? (parse "20031231")
            (:values (enc 0 0 0 31 12 2003 0) 0))

       (=>? (parse "-03-12")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (parse "-03")
            (:values (enc 0 0 0 1 1 2003 0) 0))
       (=>? (parse "-0312")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (parse "-03-12-31")
            (:values (enc 0 0 0 31 12 2003 0) 0))
       
       (=>? (parse "03-12-31")
            (:values (enc 0 0 0 31 12 2003 0) 0))

       (=>? (parse "031231")
            (:values (enc 0 0 0 31 12 2003 0) 0))

       (=>? (parse "2003335")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (parse "2003-335")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (parse "03335")
            (:values (enc 0 0 0 1 12 2003 0) 0))
       (=>? (parse "03-335")
            (:values (enc 0 0 0 1 12 2003 0) 0))

       ;; c.f. RFC 3339, 3. Two Digit Years, last item
       (=>? (parse ":0-09-09")
            (:values (enc 0 0 0 9 9 2000 0) 0))
       (=>? (parse ";0-09-09")
            (:values (enc 0 0 0 9 9 2010 0) 0))
       
       ;; supplemental this year
       (let ((this-year (nth-value 5 (decode-universal-time
                                      (get-universal-time) 0))))
         (=>? (parse "Thu, 01 Jan")
              (:values (enc 0 0 0 1 1 this-year 0) 0))
         (=>? (parse "01 Jan")
              (:values (enc 0 0 0 1 1 this-year 0) 0))
         (=>? (parse "1 Jan")
              (:values (enc 0 0 0 1 1 this-year 0) 0)))

       ;; bogus W3CDTF (invalid hour)
       (=>? (parse "2003-12-31T25:14:55Z")
            (:values (enc 55 14 1 1 1 2004 0) 0))

       ;; bogus W3CDTF (invalid minute)
       (=>? (parse "2003-12-31T10:61:55Z")
            (:values (enc 55 1 11 31 12 2003 0) 0))

       ;; bogus W3CDTF (invalid second)
       (=>? (parse "2003-12-31T10:14:61Z")
            (:values (enc 1 15 10 31 12 2003 0) 0))

       ;; MSSQL date time format
       (=>? (parse "2004-07-08 23:56:58.7")
            (:values (enc 58 56 23 8 7 2004 0) 0.7))

       ;; MSSQL-ish date time format (without fractional second)
       (=>? (parse "2004-07-08 23:56:58")
            (:values (enc 58 56 23 8 7 2004 0) 0))

       ;; above some examples from http://pythonhosted.org/feedparser/date-parsing.html
       )


;; Cleanup for Eval-Test
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features* *features-tmp*))


;;====================================================================
