;;;; Produce HTML output in a fast, cachable, modular and easy way.

(in-package :macro-html)

(defparameter *html-doctype* "<!DOCTYPE html>"
  "Default DOCTYPE.")

(defparameter *element-escapes* "<>&"
  "Characters to escape in elements.")

(defparameter *attribute-escapes* "<>&\"'"
  "Characters to escape in attributes.")

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *html-elements*
    '(a abbr address article aside audio b bdi bdo blockquote body button
      canvas caption cite code colgroup datalist dd del details dfn div
      dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4
      h5 h6 head hgroup header html i iframe ins keygen kbd label legend
      li map mark menu meter nav noscript object ol optgroup option
      output p pre progress q rp rt ruby s samp script section select
      small span strong style sub sup table tbody td textarea tfoot th
      thead time title tr u ul var video wbr)
    "List of HTML elements.")
  
  (defparameter *html-single-elements*
    '(area base br col command hr img input link meta param source track)
    "List of single HTML elements."))

(defun html-doctype ()
  "Prints HTML doctype."
  (write-string *html-doctype*))

(declaim (inline escape-char))
(defun escape-char (char)
  "Return the escape string of CHAR."
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape-string (string to-escape)
  "Return escaped string of STRING."
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
	 for pos = (position-if #'needs-escape-p string :start start)
	 do (write-sequence string out :start start :end pos)
	 when pos do (write-sequence (escape-char (char string pos)) out)
	 while pos))))

(declaim (inline escape-element))
(defun escape-element (string)
  "Escape *element-escapes* in STRING and return it."
  (escape-string string *element-escapes*))

(declaim (inline escape-attribute))
(defun escape-attribute (string)
  "Escape *attribute-escapes* in STRING and return it."
  (escape-string string *attribute-escapes*))

(defun to-string (object)
  "Converts OBJECT to string or returns nil."
  (typecase object
    (string object)
    (pathname #-(or ccl) (namestring object)
	      #+ccl (ccl:native-translated-namestring object))))

(defmacro element-attributes (attributes)
  "Generate code to print element ATTRIBUTES."
  `(progn
     ,@(loop for (key value) on attributes by #'cddr
	  collect (if value
		      `(format t " ~a=\"~a\""
			       (symbol-name ,key)
			       (escape-attribute (to-string ,value)))
		      `(format t " ~a" (symbol-name ,key))))))

(defmacro element-open (name-string attributes)
  "Generate code to print element NAME-STRING with ATTRIBUTES opening."
  `(progn
     (format t "<~a" ,name-string)
     (element-attributes ,attributes)
     (write-char #\>)
     (values)))

(declaim (inline element-close))
(defun element-close (name-string)
  "Print closing tag for element NAME-STRING."
  (format t "</~a>" name-string))

(defun parse-element-arguments (arguments)
  "Parse attributes and children in arguments."
  (let ((potential-attributes (first arguments)))
    (if (and (listp potential-attributes)
	     (eq :attributes (first potential-attributes)))
	(values (rest (first arguments)) (rest arguments))
	(values nil arguments))))

(defun read-attribute-set (stream char)
  "Reader macro function: [a b ...] -> (:attributes a b ...)."
  (cons :attributes (read-delimited-list #\] stream t)))

(defmacro print-children (children)
  `(progn
     ,@(loop for child in children collect
            `(write-string (escape-element (to-string ,child))))))

(defmacro text (&body text-nodes)
  "Print TEXT-NODES as if they were arguments to a tag macro."
  `(progn (print-children ,text-nodes)
          (values)))

(defmacro define-element (name)
  "Define an element macro."
  `(defmacro ,name (&rest arguments)
     (let ((name-string (symbol-name ',name)))
       (multiple-value-bind (attributes children)
           (parse-element-arguments arguments)
         `(progn
            (element-open ,name-string ,attributes)
	    (print-children ,children)
            (element-close ,name-string)
            (values))))))

(defmacro define-single-element (name)
  "Define a single-element macro."
  `(defmacro ,name (&rest attributes)
     `(progn
        (element-open ,(symbol-name ',name) ,attributes)
        (values))))

(defmacro define-html-elements ()
  "Defines *HTML-ELEMENTS* as elements and *HTML-SINGLE-ELEMENTS* as
single elements."
  `(progn
     ,@(loop for element in *html-elements*
	  collect `(define-element ,element))
     ,@(loop for element in *html-single-elements*
	  collect `(define-single-element ,element))
     (export *html-elements*)
     (export *html-single-elements*)
     (values)))

;;; define all HTML element macros
(define-html-elements)
