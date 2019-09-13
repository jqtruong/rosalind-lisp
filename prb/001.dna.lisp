;;;; 001.dna.lisp

(defpackage :001.dna
  (:use :cl)
  (:import-from #:strand
                #:print-base-count)
  (:export #:run
           #:*sample-dataset*
           #:*sample-output*
           #:*problem-description*))

(in-package #:001.dna)

(defvar *problem-description*
  "Given: A DNA string `STRAND' of length at most 1000 nt (nucleotides).

Return: Four integers (separated by spaces) counting the respective number of
times that the symbols `A', `C', `G', and `T' occur in `STRAND'.")

(defvar *sample-dataset*
  "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

(defvar *sample-output*
  "20 12 17 21")

(defun run (&optional (strand *sample-dataset*))
  (print-base-count strand))
