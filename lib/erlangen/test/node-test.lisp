;;;; Basic test for node protocol.

(defpackage erlangen.distribution.protocol.node-test
  (:use :cl
        :erlangen
        :erlangen.distribution.protocol.node
        :erlangen.distribution.protocol.port-mapper
        :erlangen.distribution.id)
  (:export :run-tests))

(in-package :erlangen.distribution.protocol.node-test)

(let (messages)
  (defun test-agent ()
    (setf messages nil)
    (loop do (push (receive) messages)))
  (defun test-messages ()
    messages))

(defun run-tests ()
  (let (port-mapper node-server-agent register-agent id)
    (unwind-protect
         (progn
           (setq port-mapper (spawn `(port-mapper)))
           (sleep 1)
           (multiple-value-bind (node-server port)
               (make-node-server)
             (setq node-server-agent (spawn node-server))
             (setq register-agent
                   (spawn `(register-node ,(node-name) ,port))))
           (sleep 1)

           ;; Test REMOTE-SPAWN
           (setq id (remote-spawn (host-name) (node-name) '(test-agent)
                                  "bar" :link 1))
           (assert (find-agent id) ()
                   "REMOTE-SPAWN failed.")
           (handler-case (remote-spawn
                          (host-name) (node-name) '(test-agent)
                          "nil" :invalid 1)
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-SPAWN succeeded with invalid mode.")))
           (handler-case (remote-spawn
                          (host-name) (node-name) '(42)
                          "nil" nil 1)
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-SPAWN succeeded with invalid call.")))
           (remote-spawn (host-name) (node-name) '(not-bound)
                         "nil" nil 1)

           ;; Test REMOTE-SEND
           (remote-send "hello" id)
           (sleep 1)
           (assert (equal '("hello") (test-messages)) ()
                   "REMOTE-SEND failed.")
           (let ((id (remote-spawn (host-name) (node-name) '(sleep 2)
                                   "nil" nil 1)))
             (remote-send "hello" id)
             (remote-send "hello2" id))
           (remote-send "hello" (agent-id :invalid))

           ;; Test REMOTE-LINK
           (remote-link id "foo" :link)
           (assert (equal (erlangen.agent:agent-links
                           (find-agent id))
                          '("foo" "bar")))
           (remote-link id "foo" :monitor)
           (assert (equal (erlangen.agent:agent-monitors
                           (find-agent id))
                          '("foo")))
           (handler-case (remote-link id "foo" :invalid)
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-LINK succeeded with invalid mode.")))
           (remote-link (agent-id :invalid) "foo" :link)
           (let (exit-notice
                 (invalid-id (agent-id :invalid)))
             (spawn (lambda ()
                      (link invalid-id :monitor)
                      (setf exit-notice (receive))))
             (sleep 1)
             (destructuring-bind (id status . reason) exit-notice
               (assert (equal id invalid-id))
               (assert (eq status :exit))
               (check-type reason error)))
           (let (exit-notice
                 (invalid-id (agent-id :invalid))
                 (linked (spawn (lambda () (receive)))))
             (spawn (lambda ()
                      (link linked :monitor)
                      (setf exit-notice (receive))))
             (remote-link invalid-id (agent-id linked) :link)
             (sleep 1)
             (destructuring-bind (agent status id . reason) exit-notice
               (assert (equal agent linked))
               (assert (eq status :exit))
               (assert (equal id invalid-id))
               (check-type reason error)))

           ;; Test REMOTE-UNLINK
           (remote-unlink id "foo")
           (assert (equal (erlangen.agent:agent-links
                           (find-agent id))
                          '("bar")))
           (assert (equal (erlangen.agent:agent-monitors
                           (find-agent id))
                          '()))
           (remote-unlink (agent-id :invalid) "foo")

           ;; Test REMOTE-EXIT
           (let* (exit-notice
                  (agent (find-agent id))
                  (monitor (spawn (lambda ()
                                    (link agent :monitor)
                                    (setf exit-notice (receive))))))
             (unwind-protect
                  (progn
                    (remote-exit :foo id)
                    (sleep 1)
                    (destructuring-bind (killed-agent status . reason)
                        exit-notice
                      (assert (eq agent killed-agent))
                      (assert (eq status :exit))
                      (assert (eq reason :foo))))
               (exit :kill monitor)))
           (remote-exit :bar id)
           (remote-exit :bar (agent-id :invalid)))

      (exit :kill (find-agent id))
      (exit :kill register-agent)
      (exit :kill node-server-agent)
      (exit :kill port-mapper)
      (clear-connections))))
