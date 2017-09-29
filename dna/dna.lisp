;;;; dna.lisp

(in-package #:dna)

(defvar *sample* "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

(defun count-bases (strand)
  "Given: A DNA string `STRAND' of length at most 1000 nt (nucleotides).

Return: Four integers (separated by spaces) counting the respective number of
times that the symbols `A', `C', `G', and `T' occur in `STRAND'.

Sample Output:
20 12 17 21"

  (loop for nt across strand
        counting (char-equal #\A nt) into Adenine
        counting (char-equal #\C nt) into Cytosine
        counting (char-equal #\G nt) into Guanine
        counting (char-equal #\T nt) into Thymine
        finally
           (return (list Adenine Cytosine Guanine Thymine))))
