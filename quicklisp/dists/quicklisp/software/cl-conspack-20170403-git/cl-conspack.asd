(defpackage :cl-conspack.asdf
  (:use #:cl #:asdf))

(in-package :cl-conspack.asdf)

(defsystem :cl-conspack
  :description "CONSPACK implementation for Common Lisp"
  :author "Ryan Pavlik"
  :license "NewBSD"

  :depends-on (:closer-mop :alexandria :ieee-floats :trivial-utf-8
               :fast-io :trivial-garbage)

  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "util")
   (:file "types")
   (:file "properties")
   (:file "secure")
   (:file "reftable")
   (:file "r-ref")
   (:file "headers")
   (:file "indexes")
   (:file "tmap")
   (:file "encode")
   (:file "decode")
   (:file "explain")))
