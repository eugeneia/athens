;;;; Access the file type hash table.

(in-package :file-types)


;;; Initialize hash tables

(defparameter *file-type-hash*
  (let ((hash-table (make-hash-table :test #'equalp)))
    (dolist (item *file-type-list* hash-table)
      (let ((extensions (first item))
	    (properties (cdr item)))
	(dolist (extension extensions)
	  (setf (getf (gethash extension hash-table) :tags)
		(append (getf (gethash extension hash-table) :tags)
			(getf properties :tags)))
          (when #1=(getf properties :mime)
                (setf (getf (gethash extension hash-table) :mime)
                      #1#))))))
  "Extension to property map.")

(defparameter *file-name-hash*
  (let ((hash-table (make-hash-table :test #'equalp)))
    (dolist (item *file-name-list* hash-table)
      (setf (gethash (first item) hash-table) (second item))))
  "Special file name to extension map.")


;;; Access functions

(defun file-name (file)
  "Return extension for FILE or NIL."
  (gethash (pathname-name file) *file-name-hash*))

(defun file-property (file property)
  "Return property of FILE or NIL."
  (let ((type (pathname-type file))
	(name (pathname-name file)))
    (or (and type (getf (gethash type *file-type-hash*)
			property))
	(and name (getf (gethash (file-name name) *file-type-hash*)
			property))
	(case property
	  (:tags '(:binary))
	  (:mime '("application" "octet-stream"))))))

(defun file-tags (file &optional tag)
  "→ _tags_

   *Arguments and Values:*

   _file_—a _pathname designator_.

   _tag_—a _keyword_.

   _tags_—a _list_ of _keywords_ which describe properties of a given
   _file_.

   *Description:*

   {file-tags} returns _tags_ for _file_. If _tag_ is given, {file-tags}
   acts as a predicate that tests if _tag_ is associated with _file_. In
   that case {file-tags} will return {nil} unless _tag_ would be present
   in _tags_."
  (let ((tag-list (file-property file :tags)))
    (if tag
	(when (member tag tag-list)
	  tag-list)
	tag-list)))

(defun file-mime (file)
  "→ _mime-type_

   *Arguments and Values:*

   _file_—a _pathname designator_.

   _mime-type_—a two-element _list_ containing both MIME type parts as
   _strings_ or {nil}.

   *Description:*

   {file-mime} returns the _mime-type_ of _file_ or {nil} if one could
   not be determined."
  (file-property file :mime))
