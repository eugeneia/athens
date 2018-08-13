;;;; Last modified: 2014-05-18 14:39:35 tkych

;; cl-date-time-parser/cl-date-time-parser.asd

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-date-time-parser/LICENSE


;;====================================================================
;; CL-DATE-TIME-PARSER:
;;====================================================================
;; cl-date-time-parser/
;;   cl-date-time-parser.asd
;;   date-time-parser.lisp
;;   README.md
;;   LICENSE
;;   CHANGELOG


;;====================================================================
;; System for CL-DATE-TIME-PARSER
;;====================================================================

(asdf:defsystem #:cl-date-time-parser
  :name        "Cl-Date-Time-Parser"
  :description "Parse date-time-string, and return (values universal-time fraction).
Parsable date-time-format: ISO8601, W3CDTF, RFC3339, RFC822, RFC2822, RFC5322, asctime, RFC850, RFC1036."
  :version     "0.1.03"
  :licence     "MIT License"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :depends-on  (#:alexandria #:split-sequence #:anaphora
                #:cl-ppcre #:local-time #:parse-float)
  :components  ((:file "date-time-parser"))
  )


;;====================================================================
