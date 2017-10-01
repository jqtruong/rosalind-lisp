;;;; dna.lisp

(in-package #:dna)

(defvar *sample-dataset* "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
(defvar *sample-output "20 12 17 21")

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
       (return `((:A . ,Adenine) (:C . ,Cytosine) (:G . ,Guanine) (:T . ,Thymine)))))

(defun get-count (key bases)
  (cdr (assoc key bases)))

(defun print-base-count (strand)
  (let ((bases (count-bases strand)))
    (format t "~d ~d ~d ~d"
            (cdr (assoc :A bases))
            (cdr (assoc :C bases))
            (cdr (assoc :G bases))
            (cdr (assoc :T bases)))))
