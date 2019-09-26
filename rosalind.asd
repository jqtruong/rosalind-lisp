;;;; rosalind.asd

(asdf:defsystem #:rosalind
  :description "Organized (and more FOCUSED) attack on Rosalind problems."
  :author "Jerome Truong<jerometruong@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "rosalind")
               (:module #:src
                :components ((:file "strand")))
               (:module #:prb
                :components ((:file "001.dna")
                             (:file "002.rna")
                             (:file "003.revc")))))
