;;;; System definition for file-types.

(defpackage file-types-asd
  (:use :cl :asdf))

(in-package :file-types-asd)

(defsystem file-types
  :description
  "Simple scheme to classify file types in a hierarchical fashion."
  :author "Max Rottenkolber <max@mr.gy>"
  :licence "GNU AGPL"
  :components ((:file "package")
               (:file "types" :depends-on ("package"))
               (:file "names" :depends-on ("package"))
               (:file "access" :depends-on ("package" "types" "names"))))
