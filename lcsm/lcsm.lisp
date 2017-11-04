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

(defun get-substrands (strand part1 part2 coll)
  "Return all substrings of `STRAND' (includes empty and 1-nucleotide strings,
and duplicates."
  (cond ((= 0 (length strand))
         (format t "Nothing to do.~%")
         '())
        ;; At first, `PART1' and `PART2' will be empty, so set `PART1' to
        ;; `STRAND' to begin.
        ((= 0 (+ (length part1) (length part2)))
         (get-substrands strand strand part2 coll))
        ;; Continue stripping from `PART1' into `PART2' until the latter is
        ;; the same as `STRAND'.
        ((< (length part2) (length strand))
         (get-substrands strand
                ;; `PART1' is reduced to nothing.
                (if (> (length part1) 1)
                    (subseq part1 1 (length part1))
                    "")
                ;; `PART2' grows into `STRAND'.
                (concatenate 'string part2 (subseq part1 0 1))
                (append coll (list part1 part2))))
        ((> (length strand) 2)
         (let ((next-strand (subseq strand 1 (1- (length strand)))))
           (get-substrands next-strand next-strand "" (append coll (list part1 part2)))))
        (t coll)))

(defun clean-strands (strands)
  "Return a sorted list of `STRANDS', removing the empty strings and
  duplicates."
  (sort
   (remove-duplicates
    (remove-if #'(lambda (s) (< (length s) 2)) strands)
    :test #'string=)
   #'(lambda (a b) (> (length a) (length b)))))

(defun parse (data)
  "Return a list of all possible substrings of the shortest strand in the FASTA
`DATA', tied to results of searching for each substring in each remaining
strand."

  (let* ((fasta-table (funs::make-fasta-hash-table data))
         (shortest-fasta (find-shortest-strand fasta-table))
         (shortest-strand (cdr shortest-fasta))
         (unique-subs (clean-strands (get-substrands shortest-strand "" "" '()))))
    (loop
       for subs in unique-subs
       collect
         (cons subs
               (loop
                  for key being the hash-keys in fasta-table
                  as strand = (gethash key fasta-table)
                  as pos = (search subs strand)
                  when (not (string= key (car shortest-fasta)))
                  collect (cons key pos))))))

(defun get-shared-motifs (unique-subs)
  "With a list of `UNIQUE-SUBS' constructed by `PARSE', only return the
  sub-strands that are shared among all of the other strands."
  (loop
     for (subs . searches) in unique-subs
     when (loop for (key . index) in searches
             collect index into found
             finally (return (every #'(lambda (x) x) found))) ; TODO move none-nil to
                                                         ; funs
     collect subs))

(defun get-first (data)
  "Given: A collection of k (k ≤ 100k) DNA strings of length at most 1 kbp each
in FASTA format, as `DATA'.

Return: A longest common substring of the collection. (If multiple solutions
exist, you may return any single solution."

  (let* ((parsed-data (parse data))
         (shared-motifs (get-shared-motifs parsed-data)))
    (nth 0 shared-motifs)))

;;; TODO takes too long because there are at least a hundred strands and we are
;;; starting with 2 nts.

(defun test/dyn-prog ()
  (let ((s1 "ABAB")
        (s2 "BABA"))
    (loop for i below (length s1)
          for c1 = (char s1 i)
          do (loop for j below (length s2)
                   for c2 = (char s2 j)
                   if (char= c1 c2)
                     if (or (= i 0) (= j 0))
                       collect (cons (cons i j) 1) into matrix
                   else
                     do (acons (cons (cons i j)
                                     (cdr (assoc (cons (1- i) (1- j)) matrix :test #'equalp)) matrix))))))
