;;;; rna.asd

(asdf:defsystem #:rna
  :serial t
  :description "Describe rna here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:dna)
  :components ((:file "package")
               (:file "rna")))

