;;;; mprt.asd

(asdf:defsystem #:mprt
  :description "Describe mprt here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:funs #:uniprot #:cl-ppcre)
  :components ((:file "package")
               (:file "mprt")))

