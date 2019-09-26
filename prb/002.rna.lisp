;;;; rna.lisp

(defpackage :002.rna
  (:use :cl)
  (:import-from #:strand
                #:transcribe)
  (:export #:run
           #:*sample-dataset*
           #:*sample-output*
           #:*problem-description*))

(in-package #:002.rna)

(defvar *sample-dataset* "GATGGAACTTGACTACGTAAATT")

(defun run (&optional (dna-coding-strand *sample-dataset*))
  "http://rosalind.info/problems/rna/

Given: A DNA string `DNA-CODING-STRAND' having length at most 1000 nt.

Return: The transcribed RNA string of `DNA-CODING-STRAND'.

Sample Output:
GAUGGAACUUGACUACGUAAAUU"

  (transcribe dna-coding-strand))
