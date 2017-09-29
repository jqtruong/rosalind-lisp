;;;; funs.lisp

(in-package #:funs)

(defun to-string (nts)
  "Convert a list of characters representing nucleotides in `NTS' into a string."
  (format nil "~{~a~^~}" nts))
