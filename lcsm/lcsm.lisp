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

(defun recur (s1 s2 max-len i j motifs matrix)
  (cond ((= i (length s1))
         ;; Last iteration; return longest shared motifs.
         motifs)
        ((= j (length s2))
         ;; Reached end of s2, go to next letter in s1.
         (recur s1 s2 max-len (1+ i) 0 motifs matrix))
        (t
         (let* ((c1 (char s1 (or i 0)))
                (c2 (char s2 (or j 0)))
                (prev-len (or (cdr (assoc (cons (1- i) (1- j)) matrix
                                          :test #'equalp))
                              0))
                (len (1+ prev-len)))
           (if (char= c1 c2)
               (recur s1 s2
                      (max len max-len)  ; max-len
                      i (1+ j)           ; i j
                      (if (> prev-len 0) ; motifs
                          (let* ((k (cons (- i prev-len) (- j prev-len)))
                                 (motif (assoc k motifs :test #'equal)))
                            (concatenate 'list
                                         (list (cons k (1+ (cdr motif))))
                                         (remove motif motifs :test #'equal)))
                          (acons (cons i j) len motifs))
                      (append (list (cons (cons i j) len)) matrix)) ; matrix
               (recur s1 s2 max-len
                      i (1+ j)          ; i j
                      motifs
                      (append (list (cons (cons i j) 0)) matrix))))))) ; matrix

(defun parse (data)
  "Return a list of all possible substrings of the shortest strand in the FASTA
`DATA', tied to results of searching for each substring in each remaining
strand."

  (let* ((fasta-table (funs::make-fasta-hash-table data))
         (first (loop for key being the hash-keys in fasta-table
                      for val = (gethash key fasta-table)
                      return (cons key val))))
    (loop for key being the hash-keys in fasta-table
          for val = (gethash key fasta-table)
          when (not (string= key (car first)))
            collect (recur (cdr first) val 0 0 0 '() '()))))

;; (defun get (data)
;;   "Given: A collection of k (k ≤ 100k) DNA strings of length at most 1 kbp each
;; in FASTA format, as `DATA'.

;; Return: A longest common substring of the collection. (If multiple solutions
;; exist, you may return any single solution."

;;   )
