(in-package :cl-user)

(defpackage :percent-encoding
  (:nicknames :percent)
  (:use :cl :anaphora)
  (:import-from :babel :string-to-octets :octets-to-string)
  ;; Variables
  (:export :*default-character-encoding*)
  ;; Conditions
  (:export :decode-error :invalid-% :invalid-hexdig)
  ;; Predicates
  (:export :gen-delims-p :sub-delims-p :reservedp :alphap :digitp :unreservedp
           :userinfop :reg-name-p :pcharp :queryp :fragmentp)
  ;; Encoder and decoder
  (:export :encode :decode))
