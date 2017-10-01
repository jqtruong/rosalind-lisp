;;;; fib.asd

(asdf:defsystem #:fib
  :description "Solving for http://rosalind.info/problems/fib/"
  :author "Jerome Truon <jerometruong@gmail.com>"
  :license "NE1"
  :serial t
  :depends-on (#:funs)
  :components ((:file "package")
               (:file "fib")))
