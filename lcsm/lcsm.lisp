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

(defun recur (s1 s2 max-len i j last-row motifs)
  (cond ((= i (length s1))
         ;; Last iteration; return longest shared motifs with lengths
         ;; > 1, along with lengths.
         (loop for motif in (remove-if #'(lambda (m) (= 1 (cdr m))) motifs)
               for len = (cdr motif)
               collect motif into indices
               collect len into lengths
               finally
               (return (list indices lengths))))
        ((= j (length s2))
         ;; Reached end of s2, go to next letter in s1.
         (recur s1 s2 max-len (1+ i) 0 '() motifs))
        (t
         (let* ((c1 (char s1 (or i 0)))
                (c2 (char s2 (or j 0)))
                (prev-len (or (and (> j 0) (nth (1- j) last-row))
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
                      (append (list (cons (cons i j) len)))) ;
               (recur s1 s2 max-len
                      i (1+ j)          ; i j
                      (append (list (cons (cons i j) 0)))
                      motifs))))))

(defun common-max (counts)
  (let ((maxes (loop for l in counts
                     collect (apply #'max l) into maxes
                     finally (return maxes))))
    (if (apply #'= maxes)
        (car maxes)
        (let ((updated-counts (loop with m = (apply #'max maxes)
                                   for l in counts
                                   for new-l = (remove m l)
                                   when (not (null new-l))
                                     collect new-l)))
          (if (> (length updated-counts) 1)
              (common-max updated-counts)
              nil)))))

(defun normalize (common-max lcsms)
  "Breaks `LCSMS' longer than the `COMMON-MAX' into substrands of that
length. Also filters out `LCSMS' shorter than `COMMON-MAX'."
  (loop for lcsm in lcsms
        for lc-len = (cdr lcsm)
        when (= common-max lc-len)
          collect (caar lcsm) into normalized
        when (< common-max lc-len)
          append (loop with start = (caar lcsm)
                       and end = (- (+ (caar lcsm) (cdr lcsm)) common-max)
                       for i from start upto end
                       collect i)
            into normalized
        finally
           (return normalized)))

(defun first-two (k1 k2 fasta-table)
  (let ((s1 (gethash k1 fasta-table))
        (s2 (gethash k2 fasta-table)))
    (recur s1 s2 0 0 0 '() '())))

(defun prepare (data)
  (let* ((fasta-table (funs::make-fasta-hash-table data))
         (keys (funs::hash-keys fasta-table))         
         (motifs (first-two (car keys) (cadr keys) fasta-table)))
    motifs))

(defun get-first (data)
  "Given: A collection of k (k ≤ 100k) DNA strings of length at most 1 kbp each
in FASTA format, as `DATA'.

Return: A longest common substring of the collection. (If multiple solutions
exist, you may return any single solution."
  (let* ((fasta-table (funs::make-fasta-hash-table data))
         (first (loop for key being the hash-keys in fasta-table
                      for val = (gethash key fasta-table)
                      return (cons key val)))
         (shared-motifs (loop for key being the hash-keys in fasta-table
                              for val = (gethash key fasta-table)
                              when (not (string= key (car first)))
                                collect (recur (cdr first) val 0 0 0 '() '())))
         (max (common-max (loop for motif in shared-motifs
                                collect (cadr motif))))
         (normalized (loop for motif in shared-motifs
                           collect (normalize max (car motif))))
         (lcsms (apply #'intersection normalized))
         (i (car lcsms)))     ; Pick the first index of shared motifs.
    (subseq (cdr first) i (+ i max))))
