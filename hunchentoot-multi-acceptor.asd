;;;; hunchentoot-multi-acceptor.asd

(asdf:defsystem #:hunchentoot-multi-acceptor
  :description "Multiple domain support under single hunchentoot acceptor"
  :author "Arnold Noronha <arnold@jipr.io>"
  :license  "Apache License, Version 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot)
  :components ((:file "package")
               (:file "hunchentoot-multi-acceptor")))
