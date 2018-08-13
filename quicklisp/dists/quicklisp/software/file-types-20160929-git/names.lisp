;;;; List of known default/special names.

(in-package :file-types)


;;; File name list

(defparameter *file-name-list*
  '(;; Conventions
    ("README" "txt")
    ("VERSION" "txt")
    ("COPYING" "txt")
    ("LICENSE" "txt")
    ("Makefile" "mk")
    ;; Init files
    (".emacs" "el")
    (".clisprc" "lisp")
    (".sbclrc" "lisp")))
