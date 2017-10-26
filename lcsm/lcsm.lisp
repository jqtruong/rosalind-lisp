;;;; lcsm.lisp

;;; Longest Common Shared Motif

(in-package #:lcsm)

(defvar *sample-dataset* ">Rosalind_1
GATTACA
>Rosalind_2
TAGACCA
>Rosalind_3
ATACA
")

(defvar *sample-output* "AC")

(defun find-shortest-strand (fasta-table)
  (loop for key being the hash-keys in fasta-table
     for strand = (gethash key fasta-table)
     for len = (length strand)
     for shortest = (cons key strand) then (if (< len (length (cdr shortest)))
                                               (cons key strand)
                                               shortest)
     finally (return shortest)))

(defun find-motif (data)
  "Given: A collection of k (k ≤ 100k) DNA strings of length at most 1 kbp each
in FASTA format, as `DATA'.

Return: A longest common substring of the collection. (If multiple solutions
exist, you may return any single solution.)"

  (let* ((fasta-table (funs::make-fasta-hash-table data))
         (shortest-strand (find-shortest-strand fasta-table))
         (min-len (length (cdr shortest-strand))))
    (loop for i below (1- min-len)
       do (loop
             for j from (1+ i) below min-len
             for seq = (subseq (cdr shortest-strand) i j)))))

;;; record the substrings that have already been looked at, using the shortest
;;; strand.
(let* ((fasta-table (funs::make-fasta-hash-table *sample-dataset*))
       (shortest-fasta (find-shortest-strand fasta-table))
       (shortest-strand (cdr shortest-fasta))
       (shortest-length (length shortest-strand))
       ;; use substrings of length of 2 then go higher until length of strand,
       ;; starting with the first nt ending below the last.
       (unique-subs (loop
                       for i below (1- shortest-length)
                       append (loop
                                 for j from (+ 2 i) upto shortest-length
                                 for subs = (subseq shortest-strand i j)
                                 unless (member subs unique-subs :test #'string=) collect subs)
                       into unique-subs
                       finally
                         (return unique-subs)))
       (subs-found (loop
                      for subs in unique-subs
                      collect
                        (cons subs
                              (loop
                                 for key being the hash-keys in fasta-table
                                 as strand = (gethash key fasta-table)
                                 as pos = (search subs strand)
                                 when (not (string= key (car shortest-fasta)))
                                 collect (cons key pos))))))
  subs-found)
