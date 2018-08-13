(in-package #:jpl-util)

(defun merge-alists (alists &key
		     (test #'eql test-p) (test-not nil test-not-p)
		     (merge #'append)
		     (from-end t)
		     (initial-value nil initial-value-p))
  "Merges the given list of association lists.

All unique keys are collected and represented in the returned alist.
For each key, the associated value will be the reduction of all of the
values (in sequence) for that key found in the input alists.  Values
are reduced using REDUCE with the function MERGE (defaulting to
#'APPEND), the list of collected values matching a key, and the values
of FROM-END (defaulting to T) and INITIAL-VALUE (unspecified by
default).

The order of the keys in the resulting alist is undefined."
  (when (and test-p test-not-p)
    (error "Can't specify both :TEST and :TEST-NOT."))
  (loop with test = (if test-not-p (complement test-not) test)
	for key in (remove-duplicates (reduce #'append alists :from-end t
					      :key (lambda (alist)
						     (map 'list #'car alist)))
				      :test test)
	for value = (apply #'reduce merge
			   (loop for alist in alists
				 for entry = (assoc key alist :test test)
				 unless (null entry) collect (cdr entry))
			   :from-end from-end
			   (if initial-value-p
			       `(:initial-value ,initial-value)
			       '()))
	collecting (cons key value)))

(defun assoc* (item alist &key (key #'identity) (test #'eql) (default '()))
  "Like ASSOC, but returns the CDR of the matching entry, or DEFAULT
if there was no match.  (ASSOC* ...) is a SETF-able place.

e.g.
\(defparameter *test-alist* '())
  => *TEST-ALIST*
\(assoc* 'a *test-alist*)
  => NIL
\(push 1 (assoc* 'a *test-alist*))
  => (1)
\(push 2 (assoc* 'a *test-alist*))
  => (2 1)
\(assoc* 'a *test-alist*)
  => (2 1)
*test-alist*
  => ((A 2 1))
\(setf (assoc* 'b *test-alist*) 'notalist)
  => NOTALIST
\(setf (assoc* 'a *test-alist*) 'notalist)
  => NOTALIST
\(assoc* 'a *test-alist*)
  => NOTALIST
*test-alist*
  => ((B . NOTALIST) (A . NOTALIST))"
  (let ((entry (assoc item alist :key key :test test)))
    (if (null entry)
	default
	(cdr entry))))

(define-setf-expander assoc* (item alist &key (key '#'identity) (test '#'eql) (default '())
				   &environment env)
  ;; Thanks to pjb on Freenode #lisp for his/her advice.
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion alist env)
    (with-gensyms (item% alist% key% test% default% entry% value%)
      (let ((store% (first store-vars)))
	(values `(,@vars ,item% ,alist% ,key% ,test% ,default% ,entry%)
		`(,@vals ,item ,reader-form ,key ,test ,default
			 (assoc ,item% ,alist% :key ,key% :test ,test%))
		`(,value%)
		`(progn
		   ;; We want default evaluated, but we might not use it.
		   ;; Try to muffle "defined but never used" warnings.
		   ;; (Don't have access to the body of the LET* form
		   ;; generated in the expansion; this is a hack.)
		   (let ((,default% ,default%))
		     (declare (ignore ,default%)))
		   (when (null ,entry%)
		     (setf ,entry% (cons ,item% nil))
		     (let ((,store% (list* ,entry% ,alist%)))
		       ,writer-form))
		   (setf (cdr ,entry%) ,value%))
		`(assoc* ,item% ,reader-form :key ,key% :test ,test% :default ,default%))))))

(defun remove-duplicate-properties (plist)
  "Removes properties from the given property list that are
superceded by properties with the same indicator occurring earlier in
the property list."
  (accumulate-to-list (acc)
    (iterate-plist (lambda (indicator value)
		     (acc indicator)
		     (acc value))
		   plist)))

(defun iterate-plist (visit plist)
  "Iterates the properties of the given property list, calling VISIT
with the indicator and value of each property.  As Common Lisp
property list semantics prescribe, only the first property that
matches any given property indicator is visited."
  ;; Performance note: it's way faster and less consing to search a
  ;; list of visited property indicators with MEMBER than it is to use
  ;; a hash-table, even with up to 45 unique property indicators.
  (loop with plist = plist
	with seen-indicators = '()
	until (endp plist)
	doing (let* ((indicator (pop plist))
		     (value (pop plist)))
		(unless (member indicator seen-indicators)
		  (push indicator seen-indicators)
		  (funcall visit indicator value))))
  (values))

(defun iterate-alist (visit alist &key (key #'identity) (test #'eql))
  "Iterates the entries of given association list, calling VISIT with
the CAR and CDR of each entry.  To match the semantics of ASSOC, only
the first entry that matches (according to TEST) any given CAR (after
applying KEY) is visited."
  (loop with seen-keyed-cars = '()
	for entry in alist
	for (car . cdr) = (progn (check-type* entry cons) entry)
	for keyed-car = (funcall key car)
	doing (unless (member keyed-car seen-keyed-cars :test test)
		(push keyed-car seen-keyed-cars)
		(funcall visit car cdr)))
  (values))

(defun plist->alist (plist)
  "Converts the given property list to a fresh asssociation list."
  (accumulate-to-list (acc)
    (iterate-plist (lambda (indicator value)
		     (acc (cons indicator value)))
		   plist)))

;;; FIXME: factor out code from this function and
;;; remove-duplicate-properties.
(defun alist->plist (alist &key (key #'identity) (test #'eql))
  "Converts the given association list to a fresh property list.

KEY and TEST are applied to the CARs of the entries of ALIST for
uniqueness-checking; see ITERATE-ALIST."
  (accumulate-to-list (acc)
    (iterate-alist (lambda (car cdr)
		     (acc car)
		     (acc cdr))
		   alist :key key :test test)))


(defun alist->hash-table (alist &key (test #'eql))
  (loop with out = (make-hash-table :test test :size (length alist))
	for (key . value) in alist
	unless (nth-value 1 (gethash key out))
	doing (setf (gethash key out) value)
	finally (return out)))

(defun hash-table->alist (ht)
  (loop for key being each hash-key of ht
	using (hash-value value)
	collecting (cons key value)))
