;;;; Various widget functions for common HTML template pieces.

(in-package :macro-html.widgets)
(in-readtable macro-html:syntax)

(defparameter *default-encoding* :utf-8
  "Default document encoding.")

(defparameter *default-viewport* "width=device-width, initial-scale=1"
  "Default document viewport.")

(defun html-widget-meta (&key (content-type "text/html")
			      (encoding *default-encoding*))
  "*Arguments and Values:*

   _content-type_—a _string_ denoting a MIME type. The default is
   {\"text/html\"}.

   _encoding_—a _keyword_ denoting a character encoding. The default is
   {:utf-8}.

   *Description:*

   {html-widget-meta} produces a {meta} element declaring the
   _content-type_ and _encoding_ of a HTML document."
  (meta :http-equiv "Content-Type"
	:content (format nil "~a; charset=~a"
			 content-type (symbol-name encoding))))

(defun html-widget-head (title &key stylesheets scripts feeds
                         (encoding *default-encoding*)
                         (viewport *default-viewport*))
  "*Arguments and Values:*

   _title_—a _string_.

   _stylesheets_—a _list_ of _stylesheet designators_. A _stylesheet
   designator_ is a _property list_ with the keys {:href} and {:media}
   reflecting they keys of a {stylesheet} link.

   _scripts_—a _list_ of _strings_ designating URIs to Javascript
   programs.

   _feeds_—a _list_ of _feed designators_. A _feed designator_ is a
   _property list_ with the keys {:href}, {:type} and {:title} refecting
   the keys of an {alternate} link.

   _encoding_—a _keyword_ denoting a character encoding. The default is
   {:utf-8}.

   _viewport_—a _string_ denoting the value of a {viewport} meta
   attribute. the default is {\"width=device-width, initial-scale=1\"}.

   *Description:*

   {html-widget-head} produces a {head} section including elements
   declaring _title_, _stylesheets_, _scripts_, _feeds_, _encoding_ and
   _viewport_."
  (head
   (html-widget-meta :encoding encoding)
   (when viewport
     ;; The sad state of the web requires us to add this atrocious and
     ;; non-standard meta attribute because mobile browsers will render
     ;; HTML pages unresponsively otherwise.
     (meta :name "viewport" :content viewport))
   (when title
     (title title))
   (dolist (stylesheet stylesheets)
     (link :rel "stylesheet"
	   :type "text/css"
	   :href (getf stylesheet :href)
	   :media (getf stylesheet :media)))
   (dolist (script scripts)
     (script [:type "text/javascript" :src script]))
   (dolist (feed feeds)
     (link :rel "alternate"
	   :title (getf feed :title)
	   :type (getf feed :type)
	   :href (getf feed :href)))))

(defun html-widget-document (title body &key stylesheets scripts feeds
			     (encoding *default-encoding*)
                             (viewport *default-viewport*))
  "*Arguments and Values:*

   _title_—a _string_.

   _body_—a _function designator_ for a _function_ that prints the
   document body to {*standard-output*}.

   _stylesheets_—a _list_ of _stylesheet designators_. A _stylesheet
   designator_ is a _property list_ with the keys {:href} and {:media}
   reflecting they keys of a {stylesheet} link.

   _scripts_—a _list_ of _strings_ designating URIs to Javascript
   programs.

   _feeds_—a _list_ of _feed designators_. A _feed designator_ is a
   _property list_ with the keys {:href}, {:type} and {:title} refecting
   the keys of an {alternate} link.

   _encoding_—a _keyword_ denoting a character encoding. The default is
   {:utf-8}.

   _viewport_—a _string_ denoting the value of a {viewport} meta
   attribute. the default is {\"width=device-width, initial-scale=1\"}.

   *Description:*

   {html-widget-document} produces a complete HTML document including a
   {head} section including elements declaring _title_, _stylesheets_,
   _scripts_, _feeds_, _encoding_ and _viewport_. The _body_ _function_
   is called to produce the document body."
  (html-doctype)
  (html
   (html-widget-head title
		     :stylesheets stylesheets
		     :scripts scripts
		     :encoding encoding
		     :feeds feeds)
   (body
    (funcall body))))

(defun html-widget-list (list &key (type :unordered)
                                   (to-string 'identity))
  "*Arguments and Values:*

   _list_—a _list_.

   _type_—one of {:ordered}, {:unordered} and {:definitions}. The default
   is {:unordered}.

   _to-string_—a _function designator_. The default is {identity}.

   *Description:*

   {html-widget-list} produces an ordered, unordered or definition list
   containing the items in _list_. _To-string_ is called on list items
   before they are included in the list.

   If _type_ is {:ordered} or {:unordered} an ordered or unordered list
   is produced respectively, containing the elements of _list_.

   If _type_ is {:definitions} a definition list is produced. _List_ must
   be a _list_ of two-element _lists_, the first and second elements
   being definition title and definition description respectively.
   {To-string} will be called with two arguments: {:title} or
   {:description} depending on the value type and the respective value of
   the two-element _list_."
  (ecase type
    (:unordered
     (ul
      (dolist (item list)
	(li (funcall to-string item)))))
    (:ordered
     (ol
      (dolist (item list)
	(li (funcall to-string item)))))
    (:definitions
     (dl
      (loop for (item description) in list
	 do
	   (dt (funcall to-string :title item))
	   (dd (funcall to-string :description description)))))))

(defun html-widget-input (name label &optional (type "text"))
  "*Arguments and Values:*

   _name_, _label_—_strings_.

   _type_—a _string_ denoting an _input type_¹. The default is
   {\"text\"}.

   *Description:*

   {html-widget-input} produces an {input} element of _type_ with _name_
   preceded by _label_.

   *See Also:*

   + [Input type](http://www.w3.org/TR/html5/forms.html#attr-input-type)"
  (label label)
  (br)
  (input :name name :type type))

(defun option-fields (option)
  "Returns value, label and selected of option."
  (values (first option) (second option) (eq :selected (third option))))

(defun option-group-p (list)
  "Returns nil unless LIST is an option group."
  (eq (first list) :group))

(defun html-select-options (options)
  "Helper for HTML-WIDGET-SELECT."
  (labels ((select-options (options top-level)
	     (loop for option in options
		do
		  (if (and top-level (option-group-p option))
		      (optgroup [:label (second option)]
				(select-options (rest (rest option)) nil))
		      (multiple-value-bind (value label selected)
			  (option-fields option)
			(if selected
			    (option :value value
				    :label label
				    :selected nil)
			    (option :value value
				    :label label)))))))
    (select-options options t)))

(defun html-widget-select (name label options &key multiple)
  "_options_::= {'(}{{}↓_option-group_ | ↓_option_{\\}}\\*{)}

   _option-group_::= {(}{:group} _label_ {{}↓_option_{\\}}\\*{)}

   _option_::= {(}_value_ _label_ {[}{:selected}{]}{)}

   *Arguments and Values:*

   _name_, _label_, _value_—_strings_.

   _multiple_—a _generalized boolean_. The default is _false_.

   *Description:*

   {html-widget-select} produces a {select} element _name_ preceded by
   _label_ containing {option} elements as declared in _options_.
   _Option-groups_ can be used to produce {optgroup} elements
   accordingly. If an _option_ declaration contains {:selected} as its
   third element, the resulting {option} element will be selected by
   default. If _multiple_ is true, the {select} element will allow for
   multiple selections."
  (label label)
  (br)
  (if multiple
      (select [:name name :multiple nil]
	      (html-select-options options))
      (select [:name name]
	      (html-select-options options))))

(defun html-widget-textarea (name label initial-text)
  "*Arguments and Values:*

   _name_, _label_, _initial-text_—_strings_.

   *Description:*

   {html-widget-text-area} produces a {textarea} element with _name_ and
   _initial-text_ preceded by _label_."
  (label label)
  (br)
  (textarea [:name name] initial-text))

(defun html-widget-form (action fields  &key (method "GET")
                                             (description "Submit"))
  "*Arguments and Values:*

   _action_—a _string_ denoting a _form action_¹.

   _fields_—a _function designator_ for a _function_ that prints the
   form's inputs to {*standard-output*}.

   _method_—one of {\"GET\"} or {\"POST\"}. The default is {\"GET\"}.

   _description_—a _string_. The default is {\"Submit\"}.

   *Description:*

   {html-widget-form} produces a {form} element with _fields_. The
   resulting form will be bound to _action_ and use _method_. It will
   also contain a submit button labeled with _description_.

   *See Also:*

   + 1. [Form action](http://www.w3.org/TR/html5/forms.html#attr-fs-action)"
  (form [:action action :method method]
	(p
	 (funcall fields)
	 (br)
	 (input :type "submit" :value description )
	 (input :type "reset"))))

(defun html-widget-table (head body)
  "*Arguments and Values:*

   _head_—a _list_.

   _body_—a _list_ of _lists_.

   *Description:*

   {html-widget-table} produces a {table} element with _head_ as its
   table head _body_ as its rows."
  (table
   (thead
    (tr (loop for column in head do
	     (th column))))
   (tbody (loop for row in body do
	       (tr (loop for column in row do
			(td column)))))))
