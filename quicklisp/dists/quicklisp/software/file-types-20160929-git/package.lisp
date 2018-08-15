;;;; Package definition for FILE-TYPES.

(defpackage file-types
  (:documentation
   "Simple scheme to classify file types in a hierarchical fashion.")
  (:use :cl)
  (:export :file-tags
	   :file-mime))
