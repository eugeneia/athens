(ql:quickload :athens)

(defun athens ()
  (when (intersection *command-line-argument-list*
                      '("-h" "-help" "--help")
                      :test 'string=)
    (princ "Usage: athens init <configuration>
       athens start <configuration>
       athens -h|-help|--help")
    (quit 0))
  (setf erlangen-platform.log:*text-output* *standard-output*)
  (destructuring-bind (exe command configuration)
      *command-line-argument-list*
    (declare (ignore exe))
    (apply (cond ((string= command "init") 'athens.service:initialize-database)
                 ((string= command "start") 'athens.service:athens-service)
                 (t (princ "Invalid command.")
                    (quit 0)))
           (configuration:import-configuration configuration))))

(defclass athens (ccl::application)
  ((command-line-arguments :initform nil)))

(setf ccl::*invoke-debugger-hook-on-interrupt* t)
(setf ccl::*debugger-hook*
      (lambda (condition hook)
        (declare (ignore hook))
        (etypecase condition
          (ccl::interrupt-signal-condition (quit 130)))))

(gc)

(save-application
 "bin/athens"
 :application-class 'athens
 :toplevel-function 'athens
 :error-handler :quit
 :purify t
 :prepend-kernel t)
