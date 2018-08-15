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
               (:file "importer"
                      :depends-on ("packages"))
               (:file "fetcher"
                      :depends-on ("packages"))
               (:file "service"
                      :depends-on ("packages" "importer" "fetcher")))
  :depends-on ("asdf"
               "flexi-streams"
               "ironclad"
               "drakma"
               "postmodern"
               "simple-date/postgres-glue"
               "jonathan"
               "parenscript"
               "sanitize"
               "trivial-feed"
               "configuration"
               "erlangen-platform"
               "httpd0"
               "macro-html"))
