;;;; System definition for ATHENS.

(defsystem athens
  :description "Athens is a news archiver for syndication feeds."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "packages")
               (:file "hash" :depends-on ("packages")))
  :depends-on ("trivial-feed" "trivial-utf-8" "ironclad" "drakma"))
