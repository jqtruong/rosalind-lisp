;;;; uniprot.asd

(asdf:defsystem #:uniprot
  :description "UniProt related functions."
  :author "Jerome Truong <jerometruong@gmail.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:drakma)
  :components ((:file "package")
               (:file "uniprot")))

