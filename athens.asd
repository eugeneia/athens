;;;; System definition for ATHENS.

(defsystem athens
  :description "Athens is a news archiver for syndication feeds."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "packages")
               (:file "hash"
                      :depends-on ("packages"))
               (:file "store"
                      :depends-on ("packages"))
               (:file "html-widgets"
                      :depends-on ("packages"))
               (:file "restful-responder"
                      :depends-on ("packages" "store"))
               (:file "athens"
                      :depends-on ("packages" "hash" "store")))
  :depends-on ("trivial-feed"
               "configuration"
               "q-thread-pool"
               "flexi-streams"
               "file-types"
               "ironclad"
               "drakma"
               "cl-fad"
               "postmodern"
               "simple-date"
               "net-telent-date"
               "httpd0"
               "jsown"
               "macro-html"
               "pretty-string"
               "parenscript"))
