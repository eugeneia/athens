(in-package #:common-lisp-user)

(defpackage #:jpl-util
  (:shadow #:sort
	   #:stable-sort
	   #:array-dimension)
  (:export ;; jpl-util.lisp
           #:copy-object
	   #:lambda*
	   #:combine-elements
	   #:eof-p
	   #:map-lines
	   #:read-lines
	   #:zip #:zip*
	   #:partition-set
	   #:partition-list
	   #:group-by-n
	   #:compose #:compose-1v #:nth-arg #:curry-left #:curry-right
	   #:sort
	   #:stable-sort
	   #:nsort
	   #:nstable-sort
	   #:coerce-boolean
	   #:check-bounding-indices
	   #:string-begin= #:string-end= #:string-begin-equal #:string-end-equal
	   #:subseq-displace
	   #:subseq*
	   #:any
	   #:all
	   #:duplicates-p
	   #:remove-ordered-duplicates
	   #:equivalent-hash-table-test
	   #:find-duplicates
	   #:unique-pairs
	   #:list-extract!
	   #:best
	   #:adjacent-pairs
	   #:map-adjacent-pairs
	   #:parse-sequence-type
	   #:parse-vector-type
	   #:delete-nth!
	   #:insert-at
	   #:clear-traces! #:list-traces #:with-trace
	   #:get-reasonable-real-time #:get-reasonable-run-time
	   #:shuffle #:shuffle!
	   #:defvar-unbound
	   #:twos-complement->integer #:integer->twos-complement
	   #:verbosely #:with-verbosity #:*verbose*
	   #:split-list!
	   #:cond-replace
	   #:with-safe-alloc
	   #:format-ordinal
	   #:a/an-number
	   #:english-list-format-control
	   #:fractional
	   #:ensure-type
	   #:check-type*
	   #:read-new-value
	   #:standard-deviation
	   #:mean
	   #:square
	   #:empty?
	   #:circular-list
	   #:with-extent-hooks #:with-extent-hooks%
	   #:decode-time-duration #:format-time-duration
	   #:integer-digits
	   #:integer-digit-count
	   #:set-equal
	   #:parse-progn
	   #:push-append #:push-nconc
	   #:xor
	   #:1or
	   #:with-accessors*
	   #:proper-list?
	   #:doseq
	   
	   ;; gensyms.lisp
	   #:with-gensyms
	   
	   ;; types.lisp
	   #:function-designator
	   #:extended-function-designator
	   #:function-name
	   #:designated-function
	   #:pathname-designator
	   #:array-index
	   #:array-dimension
	   #:universal-time
	   #:subsecond-universal-time
	   #:format-control
	   #:class-designator
	   #:designated-class
	   
	   ;; vector-delete.lisp
	   #:vector-delete
	   #:vector-delete-range
	   
	   ;; accumulators.lisp
	   #:accumulate-to-vector
	   #:accumulate-to-dynamic-vector
	   #:accumulate-to-list
	   #:accumulate-to-hash-table
	   
	   ;; iterators.lisp
	   #:with-list-iterator
	   #:with-vector-iterator
	   #:with-sequence-iterator
	   #:with-range-iterator
	   
	   ;; subtypecase.lisp
	   #:subtypecase #:otherwise
	   #:csubtypecase #:esubtypecase
	   #:subtype-error #:subtype-error-type
	   #:subtype-error-expected-supertype
	   
	   ;; option-clause.lisp
	   #:option-clause-bind
	   
	   ;; alists-and-plists.lisp
	   #:merge-alists
	   #:assoc*
	   #:remove-duplicate-properties
	   #:iterate-plist #:iterate-alist
	   #:plist->alist #:alist->plist
	   #:alist->hash-table #:hash-table->alist
	   
	   ;; defclass-star.lisp
	   #:defclass*
	   #:accessor-name #:accessor-type
	   #:omit-options
	   
	   ;; order.lisp
	   #:lesser?
	   #:composite-lesser?
	   #:symbol<
	   #:package<
	   #:test-order-pred)
  (:use #:common-lisp))
