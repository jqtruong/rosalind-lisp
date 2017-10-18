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

(defun calc-consensus-profile (data)
  "Returns the consensus and its profile calculated from
fasta-formatted data."
  (let* ((fasta-table (funs::make-fasta-hash-table data))
         (lim         (loop for strand being the hash-values in fasta-table
                         minimize (length strand) into lim
                         finally (return lim)))
         (columns     (loop for i below lim
                         collect (loop for strand being the hash-values in fasta-table
                                    collect (char strand i))))
         (profile     (loop for col in columns
                         collect (dna::count-bases (format nil "~{~a~}" col))))
         (consensus   (loop for base-counts in profile
                         collect (dna::get-majority-base base-counts))))
    (values consensus profile)))

(defun to-string (data)
  "Given: A collection of at most 10 DNA strings in `DATA' of equal length (at most 1 kbp) in FASTA format.

Return: A consensus string and profile matrix for the collection. (If
several possible consensus strings exist, then you may return any one
of them.)"
  (multiple-value-bind (consensus profile) (calc-consensus-profile data)
    (let ((consensus-string (loop for base-count in consensus
                               collect (car base-count) into nts
                               finally (return (format nil "~{~A~}" nts))))
          (profile-string   (loop for base-counts in profile
                               collect (cdr (assoc #\A base-counts)) into As
                               collect (cdr (assoc #\C base-counts)) into Cs
                               collect (cdr (assoc #\G base-counts)) into Gs
                               collect (cdr (assoc #\T base-counts)) into Ts
                               finally
                                 (return
                                   (format nil
                                           "A: ~{~A~^ ~}~%C: ~{~A~^ ~}~%G: ~{~A~^ ~}~%T: ~{~A~^ ~}"
                                           As            Cs            Gs            Ts)))))
      (format nil "~A~%~A" consensus-string profile-string))))

;;; Tests
(format t "
;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;
Consensus and Profile match: ~a
"
        (string-equal *sample-output* (to-string *sample-dataset*)))
