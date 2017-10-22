;;;; grph.lisp

(in-package #:grph)

(defvar *sample-dataset* ">Rosalind_0498
AAATAAA
>Rosalind_2391
AAATTTT
>Rosalind_2323
TTTTCCC
>Rosalind_0442
AAATCCC
>Rosalind_5013
GGGTGGG
")

(defvar *sample-output* "Rosalind_0498 Rosalind_2391
Rosalind_0498 Rosalind_0442
Rosalind_2391 Rosalind_2323")

(defun adjacency-list (data k)
  "Given: A collection of DNA strings in FASTA format having total length at most 10 kbp.

Return: The adjacency list corresponding to O(k). You may return edges in any
order."

  (let ((fasta-table (funs::make-fasta-hash-table data)))
    (loop for k1 being the hash-keys in fasta-table
       for v1 = (gethash k1 fasta-table)
       for s1 = (subseq v1 (- (length v1) k))
       collect (cons k1 (loop for k2 being the hash-keys in fasta-table
                           for v2 = (gethash k2 fasta-table)
                           for s2 = (subseq v2 0 k)
                           when (and (not (string= k1 k2))
                                     (string= s1 s2))
                           collect  k2)))))

;;;--- TODO make a macro of `to-string'.
(defun to-string (data k)
  (let ((adj-l (adjacency-list data k)))
    (loop for node-nodes in adj-l
       for node  = (car node-nodes)
       for nodes = (cdr node-nodes)
       when nodes
       do (loop for adj-node in nodes
             do (format t "~a ~a~%" node adj-node)))))
