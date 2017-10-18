;;;; cons.lisp

(in-package #:cons)

(defvar *sample-dataset* ">Rosalind_1
ATCCAGCT
>Rosalind_2
GGGCAACT
>Rosalind_3
ATGGATCT
>Rosalind_4
AAGCAACC
>Rosalind_5
TTGGAACT
>Rosalind_6
ATGCCATT
>Rosalind_7
ATGGCACT
")

(defvar *sample-output* "ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6")

(defun calc-consensus (data)
  "Given: A collection of at most 10 DNA strings in `DATA' of equal length (at most 1 kbp) in FASTA format.

Return: A consensus string and profile matrix for the collection. (If
several possible consensus strings exist, then you may return any one
of them.)"

  (let ((fasta-table (funs::make-fasta-hash-table data)))
    (loop for strand being the hash-values in fasta-table
       do
         ;; need to transpose the strings then count bases
         (format t "~a~%" strand))))