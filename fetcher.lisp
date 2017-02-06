(in-package :athens.service)

(defun map-spawn (function list &key (level 1)
                  &aux (active-agents 0))
  (flet ((spawn-agent (element)
           (spawn (funcall function element) :attach :monitor)
           (incf active-agents))
         (join-agent ()
           (receive)
           (decf active-agents)))
    (loop for element in list            do (spawn-agent element)
          unless (< active-agents level) do (join-agent))
    (loop while  (> active-agents 0)     do (join-agent)))
  (values))

(defmacro with-flexi-use-value (&body body)
  "Bind FLEXI-STREAM's USE-VALUE restart during BODY."
  `(handler-bind
       ((external-format-encoding-error
         #'(lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'use-value #\Replacement_Character))))
     ,@body))

(defun headers (date)
  "Construct headers for «If-Modified-Since» request for DATE and
includes «Accept-Charset» to favor UTF-8."
  `(("If-Modified-Since" . ,(universal-time-to-http-date date))
    ("Accept-Charset" . "utf-8, *:q=0.5" )))

(define-condition not-modified (condition) ())

(defun http-get (url date timeout)
  (with-flexi-use-value
    (multiple-value-bind (body status)
        (http-request url
                      :method :get
                      :redirect 1
                      :additional-headers (headers date)
                      :deadline (+ (* timeout internal-time-units-per-second)
                                   (get-internal-real-time)))
      (ecase status
        (200 (etypecase body
               (string body)
               ((array (unsigned-byte 8)) (octets-to-string body))))
        (304 (signal 'not-modified))))))

(defun pull-feed (url if-modified-since timeout)
  (handler-case (parse-feed (http-get url if-modified-since timeout))
    (:no-error (feed)
      (write-log `(:got ,url))
      (cast :importer (list* :source url feed)))
    (not-modified (condition)
      (declare (ignore condition))
      (write-log `(:not-modified ,url)))
    (error (error)
      (write-log `(:fail ,url ,error)))))

(defun pull-feeds (db-configuration concurrent-requests get-timeout)
  (with-database db-configuration
    (map-spawn (lambda (hash)
                 (destructuring-bind (&key source date &allow-other-keys)
                     (get-feed hash)
                   `(pull-feed ,source ,date ,get-timeout)))
               (feed-index)
               :level concurrent-requests)))

(defun feed-fetcher (db-configuration &key (pull-interval 3600)
                                           (concurrent-requests 10)
                                           (get-timeout 5))
  (send `(:pull ,(agent) :repeat ,pull-interval) :fetch-timer)
  (loop do (ecase (receive :timeout (* pull-interval 2))
             (:pull (write-log :pull)
                    (pull-feeds db-configuration
                                concurrent-requests
                                get-timeout)))))
