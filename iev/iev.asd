;;;; iev.asd

(asdf:defsystem #:iev
  :description "Describe iev here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:iprb)
  :components ((:file "package")
               (:file "iev")))

