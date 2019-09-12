;;;; 001.dna.lisp

(defpackage :001.dna
  (:use :cl)
  (:import-from #:strand
                #:count-bases
                #:get-count)
  (:export #:print-base-count
           #:*sample-dataset*
           #:*sample-output*))

(in-package #:001.dna)

(defvar *sample-dataset*
  "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

(defvar *sample-output*
  "20 12 17 21")

(defun print-base-count (strand)
  (let ((bases (count-bases strand)))
    (format t "~d ~d ~d ~d"
            (get-count #\A bases)
            (get-count #\C bases)
            (get-count #\G bases)
            (get-count #\T bases))))
