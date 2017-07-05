;;;; skip-list.asd

(asdf:defsystem #:skip-list
  :description "Skip List Implementation"
  :author "Kevin Ednalino <kcednalino@gmail.com>"
  :license "Apache License 2.0"
  :depends-on ()
  :serial t
  :components ((:file "src/package")
               (:file "src/skip-list")))
