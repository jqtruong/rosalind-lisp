;;;; cons.asd

(asdf:defsystem #:cons
  :description "Describe cons here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:dna
               #:funs)
  :components ((:file "package")
               (:file "cons")))
