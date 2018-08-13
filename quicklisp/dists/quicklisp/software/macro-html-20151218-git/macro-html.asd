;;;; System definition for MACRO-HTML.

(defsystem macro-html
  :description
  "HTML generation library. Aims to be fast, modular, cachable and
concise. It does so by defining each tag as a macro which expands to
code printing the respective HTML source. Also employs a DSL for
element attributes."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :version "1.2"
  :components ((:file "packages")
	       (:file "macro-html" :depends-on ("packages"))
	       (:file "syntax" :depends-on ("packages"
                                            "macro-html"))
	       (:file "widgets" :depends-on ("packages"
                                             "macro-html"
                                             "syntax")))
  :depends-on ("named-readtables"))
