;;;; Athens RESTful responder for use with HTTPD0.

(in-package :athens.restful-responder)

;; Ad-hoc HTTP router
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compile-route-predicate (resource)
  (if (wild-pathname-p resource)
      `(pathname-match-p resource ,resource)
      `(equal resource ,resource)))

(defun compile-route-arguments (resource parameters headers)
  `(,@(when (wild-pathname-p resource :name)
        '((pathname-name resource)))
    ,@(when (wild-pathname-p resource :type)
        '((pathname-type resource)))
    ,@(loop for parameter in parameters collect
           `(cdr (assoc ,(symbol-name parameter) parameters
                        :test 'string-equal)))
    ,@(loop for header in headers collect
           `(,header headers))))

(defun compile-routes (routes)
  `(cond ,@(loop for (() resource function parameters headers) in routes
              collect `(,(compile-route-predicate resource)
                        (funcall ',function ,@(compile-route-arguments
                                               resource parameters headers))))
         (t (respond-not-found))))

(defun applicable-routes (method routes)
  (remove-if-not (lambda (route-methods)
                   (member method route-methods))
                 routes
                 :key 'first))

(defmacro define-router (name &body routes)
  `(defun ,name (resource parameters headers)
     (case *request-method*
       ,@(loop for method in '(:get :head :post) collect
              `(,method
                ,(compile-routes (applicable-routes method routes)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

(defparameter *html-mime* '("text" "html; charset=utf-8"))
(defparameter *json-mime* '("application" "json"))
(defparameter *script-mime* '("text" "javascript; charset=utf-8"))
(defparameter *style-mime* '("text" "css; charset=utf-8"))

(defun render-widget (widget argument)
  (with-output-to-vector (*standard-output* () :external-format :utf-8)
    (funcall widget argument)))

(defun render-feed-html (feed)
  (render-widget 'html-widget-feed feed))

(defun render-item-html (item)
  (render-widget 'html-widget-item item))

(defun render-news-html (news)
  (render-widget 'html-widget-news news))

(defun render-json (object)
  ;; Jonathan’s :OCTETS parameter is broken
  ;(to-json object :octets t)
  (string-to-utf-8-bytes (to-json object)))

(defun serve-object (object date
                     formatter mime-type)
  (let ((octets (funcall formatter object)))
    (respond-ok ((length octets) mime-type date)
      (write-sequence octets *standard-output*))))

(defun serve-object-if-modified (object date if-modified-since
                                 formatter mime-type)
  (if (and if-modified-since (>= if-modified-since date))
      (respond-not-modified)
      (serve-object object date formatter mime-type)))

(defun serve-content (content if-modified-since formatter mime-type)
  (if content
      (serve-object-if-modified content (getf content :date) if-modified-since
                                formatter mime-type)
      (respond-not-found)))

(defun serve-feed (hash if-modified-since formatter mime-type)
  (serve-content (get-feed hash) if-modified-since formatter mime-type))

(defun serve-feed-html (hash if-modified-since)
  (serve-feed hash if-modified-since 'render-feed-html *html-mime*))

(defun serve-feed-json (hash if-modified-since)
  (serve-feed hash if-modified-since 'render-json *json-mime*))

(defun serve-item (hash if-modified-since formatter mime-type)
  (serve-content (get-item hash) if-modified-since formatter mime-type))

(defun serve-item-html (hash if-modified-since)
  (serve-item hash if-modified-since 'render-item-html *html-mime*))

(defun serve-item-json (hash if-modified-since)
  (serve-item hash if-modified-since 'render-json *json-mime*))

(defun serve-news (start end formatter mime-type
                   &aux
                     ;; END defaults to now.
                     (end (or (and end (parse-integer end))
                              (get-universal-time)))
                     ;; START defaults 24 hours in the past.
                     (start (or (and start (parse-integer start))
                                (- end 86400))))
  (serve-object `(:start ,start :end ,end :items ,(get-items start end)) end
                formatter mime-type))

(defun serve-news-html (start end)
  (serve-news start end 'render-news-html *html-mime*))

(defun serve-news-json (start end)
  (serve-news start end 'render-json *json-mime*))

(defun compile-static-widget (widget)
  (values (with-output-to-vector (*standard-output* () :external-format :utf-8)
            (funcall widget))
          (get-universal-time)))

(defun respond-static (payload timestamp if-modified-since mime-type)
  (if (and if-modified-since (>= if-modified-since timestamp))
      (respond-not-modified)
      (respond-ok ((length payload) mime-type timestamp)
        (write-sequence payload *standard-output*))))

(multiple-value-bind (payload timestamp)
    (compile-static-widget 'html-widget-frontend)
  (defun serve-frontend (if-modified-since)
    (respond-static payload timestamp if-modified-since *html-mime*)))

(multiple-value-bind (payload timestamp)
    (compile-static-widget 'css-widget-style)
  (defun serve-style (if-modified-since)
    (respond-static payload timestamp if-modified-since *style-mime*)))

(define-router athens-router
  ((:get :head) #p"feed/*.html" serve-feed-html  () (if-modified-since))
  ((:get :head) #p"feed/*.json" serve-feed-json  () (if-modified-since))
;  ((:post)      #p"feed"        handle-add-feeds () ())
  ((:get :head) #p"item/*.html" serve-item-html  () (if-modified-since))
  ((:get :head) #p"item/*.json" serve-item-json  () (if-modified-since))
  ((:get :head) #p"news.html"   serve-news-html  (start end) ())
  ((:get :head) #p"news.json"   serve-news-json  (start end) ())
;  ((:get :head) #p""            serve-frontend   () (if-modified-since))
;  ((:get :head) #p"script"      serve-script     () (if-modified-since))
  ((:get :head) #p"style"       serve-style      () (if-modified-since)))

(defun athens-respond (connection)
  (http-respond connection 'athens-router))

(defun athens-responder (socket database)
  (with-database database
    (socket-responder socket #'athens-respond)))
