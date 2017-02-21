;;;; Configuration file mechanism.
;;;; syntax: [<KEY> <VALUE>] ...

(defpackage configuration
  (:use :cl)
  (:export :import-configuration
           :persist-configuration))

(in-package :configuration)

(defun import-configuration (path)
  "Read configuration from PATH and return it as a plist."
  (with-open-file (in path)
    (with-standard-io-syntax
      (loop for key = (read in nil 'eof) until (eq key 'eof)
         collect key collect (read in)))))

(defun persist-configuration (configuration path
                              &key (if-exists :error))
  "Write CONFIGURATION plist to PATH."
  (with-open-file (out path :direction :output :if-exists if-exists)
    (with-standard-io-syntax
      (loop for rest = configuration then (cddr rest)
         while rest do (format out "~S ~S~%" (first rest) (second rest))))))
