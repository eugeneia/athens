;;; namespace.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:libxml2.tree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct %xmlNs
  ;; struct _xmlNs *	next	: next Ns link for this node
  (%next %xmlNsPtr)
  ;; xmlNsType	type	: global or local
  (%type %xmlNsType)
  ;; const xmlChar *	href	: URL for the namespace
  (%href %xmlCharPtr)
  ;; const xmlChar *	prefix	: prefix for the namespace  
  (%prefix %xmlCharPtr)
  ;; void *	_private	: application data
  (%_private :pointer)
  ;; struct _xmlDoc *	context	: normally an xmlDoc  
  (%context %xmlDocPtr))

(defwrapper ns %xmlNs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search-ns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libxml2-function ("xmlSearchNs" %xmlSearchNs) %xmlNsPtr
  (doc %xmlDocPtr)
  (node %xmlNodePtr)
  (prefix %xmlCharPtr))

(defun search-ns-by-prefix (element prefix)
  (with-garbage-pool ()
    (let ((%ns (%xmlSearchNs (pointer (document element))
                             (pointer element)
                             (if prefix
                                 (cleanup-register (foreign-string-alloc prefix) #'foreign-string-free)
                                 (null-pointer)))))
      (unless (null-pointer-p %ns)
        (make-instance 'ns :pointer %ns)))))
  

(define-libxml2-function ("xmlSearchNsByHref" %xmlSearchNsByHref) %xmlNsPtr
  (doc %xmlDocPtr)
  (node %xmlNodePtr)
  (href %xmlCharPtr))

(defun search-ns-by-href (element href)
  (with-garbage-pool ()
    (let ((%ns (%xmlSearchNsByHref (pointer (document element))
                                   (pointer element)
                                   (if href
                                       (cleanup-register (foreign-string-alloc href) #'foreign-string-free)
                                       (null-pointer)))))
      (unless (null-pointer-p %ns)
        (make-instance 'ns :pointer %ns)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-ns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun generate-ns-prefix (element)
  (iter (for i from 1)
        (for prefix = (format nil "ns_~A" i))
        (finding prefix such-that (null-pointer-p (with-foreign-string (%prefix prefix)
                                                    (%xmlSearchNs (pointer (document element))
                                                                  (pointer element)
                                                                  %prefix))))))

(define-libxml2-function ("xmlNewNs" %xmlNewNs) %xmlNsPtr
  (node %xmlNodePtr)
  (href %xmlCharPtr)
  (prefix %xmlCharPtr))

(defun make-ns (element href &optional prefix)
  (make-instance 'ns
                 :pointer (with-foreign-strings ((%href href) (%prefix (or prefix (generate-ns-prefix element))))
                            (%xmlNewNs (pointer element)
                                       %href
                                       %prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; namespace-uri
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric namespace-uri (obj))

(defmethod namespace-uri ((ns ns))
  (foreign-string-to-lisp (wrapper-slot-value ns '%href)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; namespace-prefix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric namespace-prefix (obj))

(defmethod namespace-prefix ((ns ns))
  (foreign-string-to-lisp (wrapper-slot-value ns '%prefix)))
