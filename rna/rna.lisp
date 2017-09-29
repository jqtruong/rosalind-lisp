;;;; rna.lisp

(in-package #:rna)

(defvar *sample* "GATGGAACTTGACTACGTAAATT")

(defun convert-thymine-to-uracil (dna-coding-strand)
  "Given: A DNA string `DNA-CODING-STRAND' having length at most 1000 nt.

Return: The transcribed RNA string of `DNA-CODING-STRAND'.

Sample Output:
GAUGGAACUUGACUACGUAAAUU"

  (substitute #\U #\T (string-upcase dna-coding-strand)))
