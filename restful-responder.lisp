;;;; Athens RESTful responder for use with HTTPD0.

(in-package :athens.restful-responder)

(defun parse-request (request)
  "Parse REQUEST and return resource arguments and type."
  (let ((directory (remove-if-not #'stringp
                                  (pathname-directory request)))
        (name (pathname-name request))
        (type-string (pathname-type request)))
    (values
     ;; Resource may be one of "feed" or "item".
     (let ((resource-string (first directory)))
       (cond ((string-equal "feed" resource-string) :feed)
             ((string-equal "item" resource-string) :item)))
     ;; Arguments are the list of strings between resource and type.
     `(,@(rest directory) ,name)
     ;; Type may be one of "json" or "html".
     (cond
       ((string-equal "json" type-string) :json)
       ((string-equal "html" type-string) :html)))))

(defun get-feed-response (hash)
  "Get feed response for HASH."
  (let ((feed (get-feed hash)))
    (if feed
        (values :ok feed (getf feed :date))
        :not-found)))

(defun get-item-response (hash)
  "Get item response for HASH."
  (let ((item (get-item hash)))
    (if item
        (values :ok item (getf item :date))
        :not-found)))

(defun response-values (resource arguments)
  "Get response values for RESOURCE with ARGUMENTS."
  (handler-case
      (ecase resource
        (:feed (apply #'get-feed-response arguments))
        (:item (apply #'get-item-response arguments)))
    ;; On failure to generate response return :NOT-FOUND.
    (error (error)
      (declare (ignore error))
      :not-found)))

(defmethod to-json ((object symbol))
  "TO-JSON implementation for symbols."
  (to-json (string-downcase (symbol-name object))))

(defun print-plist-json (plist)
  "Format PLIST as JSON to *STANDARD-OUTPUT*."
  (write-string
   (to-json `(:obj ,@(loop for head = plist then (cddr head)
                           for key = (car head)
                           for value = (cadr head)
                        while head
                        collect (cons key value))))))

(defun response-formatter (resource type)
  "Get response formatter for RESOURCE and TYPE."
  (ecase type
    (:json #'print-plist-json)
    (:html (ecase resource
             (:feed #'html-widget-feed)
             (:item #'html-widget-item)))))

(defun handle-request (request if-modified-since)
  "Handle request for REQUEST and IF-MODIFIED-SINCE."
  (if (and if-modified-since
           (>= if-modified-since (get-global-date)))
      :not-modified
      (multiple-value-bind (resource arguments type)
          (parse-request request)
        ;; If request is invalid RESOURCE or TYPE will be NIL.
        (if (and resource type)
            ;; REQUEST is valid. Get RESPONSE-VALUES and
            ;; RESPONSE-FORMATTER.
            (multiple-value-bind (status datum write-date)
                (response-values resource arguments)
              (values status datum write-date 
                      type (response-formatter resource type)))
            ;; REQUEST is invalid. Return :NOT-FOUND.
            :not-found))))

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
