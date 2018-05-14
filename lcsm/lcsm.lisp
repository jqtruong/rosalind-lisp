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

(defun recur (s1 s2 i j last-rows motifs)
  (cond ((= i (length s1))
         ;; Last iteration; return longest shared motifs with lengths
         ;; > 1, along with lengths.
         (loop for motif in (remove-if #'(lambda (m) (= 1 (cdr m))) motifs)
               for len = (cdr motif)
               collect motif into indices ; of shared motifs
               unless (member len lengths)
                 collect len into lengths ; of unique lengths
               finally
                  (return (list indices (sort lengths #'>)))))
        ((= j (length s2))
         ;; Reached end of s2, go to next letter in s1.
         (recur s1 s2 (1+ i) 0 (list (cadr last-rows) '()) motifs))
        (t
         (let* ((c1 (char s1 (or i 0)))
                (c2 (char s2 (or j 0)))
                (last-row (car last-rows))
                (this-row (cadr last-rows))
                (prev-len (or (and (> j 0)
                                   (nth (1- j) last-row))
                              0))
                (len (1+ prev-len)))
           (if (char= c1 c2)
               (recur s1 s2
                      i (1+ j)           ; i j
                      (list last-row (append this-row (list len)))
                      (if (> prev-len 0) ; motifs
                          (let* ((k (cons (- i prev-len) (- j prev-len)))
                                 (motif (assoc k motifs :test #'equal)))
                            (concatenate 'list
                                         (list (cons k (1+ (cdr motif))))
                                         (remove motif motifs :test #'equal)))
                          (acons (cons i j) len motifs)))
               (recur s1 s2
                      i (1+ j)
                      (list last-row (append this-row '(0)))
                      motifs))))))

(defun prepare (data)
  (let* ((fasta-table (funs::make-fasta-hash-table data))
         (keys (funs::hash-keys fasta-table))
         (s1 (gethash (car keys) fasta-table))
         (s2 (gethash (cadr keys) fasta-table))
         (shared-motifs (recur s1 s2 0 0 '() '())))
    (list fasta-table keys shared-motifs)))

(defun get-lcsm (data)
  "Given: A collection of k (k ≤ 100k) DNA strings of length at most 1 kbp each
in FASTA format, as `DATA'.

Return: A longest common substring of the collection. (If multiple solutions
exist, you may return any single solution."
  (let* ((parsed-data (prepare data))
         (fasta-table (car parsed-data))
         (keys (cadr parsed-data))
         (1st-strand (gethash (car keys) fasta-table))
         (shared-motifs (caddr parsed-data))
         (positions (car shared-motifs))
         (lengths (cadr shared-motifs)))
    (loop for len in lengths
          for lcsm = (loop for pos-of-len in (remove-if-not #'(lambda (pos) (= len (cdr pos)))
                                                            positions)
                           for i = (caar pos-of-len)
                           for motif = (subseq 1st-strand i (+ i len))
                           when (loop for key in (cddr keys)
                                      for strand = (gethash key fasta-table)
                                      unless (search motif strand)
                                        return nil
                                      finally
                                         (return t))
                             return motif)
          when lcsm
            return lcsm)))
