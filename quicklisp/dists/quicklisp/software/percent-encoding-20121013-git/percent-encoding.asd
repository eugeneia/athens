(defpackage :percent.asd (:use :cl :asdf))
(in-package :percent.asd)

(defsystem :percent-encoding
  :version "0.1"
  :author "Manabu Takayama <learn.libra@gmail.com>"
  :license "MIT License"
  :depends-on (:anaphora :babel)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "type")
                                     (:file "var")
                                     (:file "util")
                                     (:file "condition")
                                     (:file "ascii")
                                     (:file "pred")
                                     (:file "encode")
                                     (:file "decode")))))

(defsystem :percent-encoding-test
  :depends-on (:percent-encoding :fiveam)
  :components ((:module "t"
                        :serial t
                        :components ((:file "package")
                                     (:file "test")))))

(defmethod perform ((o test-op) (c (eql (find-system :percent-encoding))))
  (asdf:load-system :percent-encoding-test)
  (funcall (intern "RUN!" :5am) (intern "ALL" :percent.test)))
