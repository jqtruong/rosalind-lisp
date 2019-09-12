;;;; strand.lisp

(defpackage :strand
  (:use :cl)
  (:export
   #:count-bases
   #:get-count))

(in-package #:strand)

(defun get-count (key bases)
  (cdr (assoc key bases)))

(defun get-majority-base (base-counts)
  (reduce #'(lambda (a b) 
              (cond ((> (cdr a) (cdr b)) a)
                    (t b)))
          base-counts))

(defun count-bases (strand)
  "Given: A DNA string `STRAND' of length at most 1000 nt (nucleotides).

Return: Four integers (separated by spaces) counting the respective number of
times that the symbols `A', `C', `G', and `T' occur in `STRAND'."

  (loop for nt across strand
     counting (char-equal #\A nt) into Adenine
     counting (char-equal #\C nt) into Cytosine
     counting (char-equal #\G nt) into Guanine
     counting (char-equal #\T nt) into Thymine
     finally
       (return `((#\A . ,Adenine) (#\C . ,Cytosine) (#\G . ,Guanine) (#\T . ,Thymine)))))
