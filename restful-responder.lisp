;;;; Athens RESTful responder for use with HTTPD0.

(in-package :athens.restful-responder)

(defun parse-request (request)
  "Parse REQUEST and return resource arguments and type."
  (let ((path (remove-if-not #'stringp
                             `(,@(pathname-directory request)
                                 ,(pathname-name request))))
        (type-string (pathname-type request)))
    (values
     ;; Resource may be one of "feed", "item" or "news".
     (let ((resource-string (first path)))
       (cond ((string-equal "feed" resource-string) :feed)
             ((string-equal "item" resource-string) :item)
             ((string-equal "news" resource-string) :news)))
     ;; Arguments are the rest of PATH.
     (rest path)
     ;; Type may be one of "json" or "html".
     (cond
       ((string-equal "json" type-string) :json)
       ((string-equal "html" type-string) :html)))))

(defun get-feed-response (if-modified-since hash)
  "Get feed response for HASH with respect to IF-MODIFIED-SINCE."
  (let ((feed (get-feed hash)))
    (if feed
        (let ((timestamp (getf feed :date)))
          (if (and if-modified-since
                   (>= if-modified-since timestamp))
              :not-modified
              (values :ok feed timestamp)))
        :not-found)))

(defun get-item-response (if-modified-since hash)
  "Get item response for HASH with respect to IF-MODIFIED-SINCE."
  (let ((item (get-item hash)))
    (if item
        (let ((timestamp (getf item :date)))
          (if (and if-modified-since
                   (>= if-modified-since timestamp))
              :not-modified
              (values :ok item timestamp)))
        :not-found)))

(defun get-news (start end)
  "Get NEWS for timespan defined by START and END."
  (let ((imports (get-imports start end)))
    (when imports
      (sort (get-items (loop for import in imports append
                            (loop for feed in import append
                                 (second feed))))
            (lambda (x y)
              (< (getf x :date) (getf y :date)))))))

(defun get-news-response (if-modified-since &optional start end)
  "Get news responce for START and END with respect to
IF-MODIFIED-SINCE."
  (let* (;; END defaults to now.
         (end (or end (get-universal-time)))
         ;; START defaults 24 hours in the past.
         (start (or start (- end 86400))))
    ;; Handle IF-MODIFIED-SINCE.
    (if (and if-modified-since
             (or (>= if-modified-since end)
                 (>= if-modified-since (get-global-date))))
        :not-modified
        (values :ok (list start end (get-news start end)) end))))

(with-database (getf cl-user::*athens-conf* :database)
  (get-news-response 0))

(defun response-values (resource arguments if-modified-since)
  "Get response values for RESOURCE with ARGUMENTS with respect to
IF-MODIFIED-SINCE."
  (handler-case
      (ecase resource
        (:feed (apply #'get-feed-response if-modified-since arguments))
        (:item (apply #'get-item-response if-modified-since arguments))
        (:news (apply #'get-news-response if-modified-since arguments)))
    ;; On failure to generate response return :NOT-FOUND.
    (error (error)
      (declare (ignore error))
      :not-found)))

(defmethod to-json ((object symbol))
  "TO-JSON implementation for symbols."
  (to-json (string-downcase (symbol-name object))))

(defun plist-jsown (plist)
  "Format PLIST for JSOWN."
  `(:obj ,@(loop for head = plist then (cddr head)
                 for key = (car head)
                 for value = (cadr head)
              while head
              collect (cons key value))))

(defun format-plist-json (plist)
  "Format PLIST as JSON to *STANDARD-OUTPUT*."
  (write-string (to-json (plist-jsown plist))))

(defun format-news-json (news)
  "Format NEWS as JSON to *STANDARD-OUTPUT*."
  (destructuring-bind (start end items) news
    (write-string
     (to-json `(:obj ("start" . ,start)
                     ("end" . ,end)
                     ("items" . ,(loop for plist in items
                                    collect (plist-jsown plist))))))))

(defun response-formatter (resource type)
  "Get response formatter for RESOURCE and TYPE."
  (ecase type
    (:json (ecase resource
             ((:feed :item) #'format-plist-json)
             (:news #'format-news-json)))
    (:html (ecase resource
             (:feed #'html-widget-feed)
             (:item #'html-widget-item)
             (:news #'html-widget-news)))))

(defun handle-request (request if-modified-since)
  "Handle request for REQUEST and IF-MODIFIED-SINCE."
  (multiple-value-bind (resource arguments type)
      (parse-request request)
    ;; If request is invalid RESOURCE or TYPE will be NIL.
    (if (and resource type)
        ;; REQUEST is valid. Get RESPONSE-VALUES and RESPONSE-FORMATTER.
        (multiple-value-bind (status datum write-date)
            (response-values resource arguments if-modified-since)
          (values status datum write-date 
                  type (response-formatter resource type)))
        ;; REQUEST is invalid. Return :NOT-FOUND.
        :not-found)))

(defun serve (datum write-date type formatter)
  "Serve DATUM of TYPE using FORMATTER."
  (let ((payload (string-to-utf-8-bytes
                  (with-output-to-string (*standard-output*)
                    (funcall formatter datum)))))
    (respond-ok ((length payload)
                 (ecase type
                   (:json '("application" "json; charset=utf-8"))
                   (:html '("text" "html; charset=utf-8")))
                 write-date)
      (write-sequence payload *standard-output*))))

(defun make-athens-responder (configuration)
  "Return RESTful responder for Athens using CONFIGURATION."
  (let ((db-configuration (getf configuration :database)))
    (lambda (request if-modified-since)
      (multiple-value-bind (status datum write-date type formatter)
          (with-database db-configuration
            (handle-request request if-modified-since))
        (case status
          (:ok (serve datum write-date type formatter))
          (:not-modified (respond-not-modified))
          (otherwise (respond-not-found)))))))
