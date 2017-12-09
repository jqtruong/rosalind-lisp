;;;; lia.asd

(asdf:defsystem #:lia
  :description "Describe lia here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:funs)
  :components ((:file "package")
               (:file "lia")))

