;;;; System definition for configuration.

(defpackage configuration-asd
  (:use :cl :asdf))

(in-package :configuration-asd)

(defsystem configuration
  :components ((:file "configuration")))
