;;;; gc.lisp

(in-package #:gc)

(defvar *sample-dataset* ">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT
")

(defvar *sample-output* "Rosalind_0808
60.919540")

(defvar *sample-gc-content-value* 60.919540)

(defun calc-gc-content (strand)
  "Return the ratio of Guanine and Cytosine in a strand."
  (let* ((bases (dna::count-bases strand))
         (As (dna::get-count #\A bases))
         (Cs (dna::get-count #\C bases))
         (Gs (dna::get-count #\G bases))
         (Ts (dna::get-count #\T bases)))
    (* (/ (+ Gs Cs)
          (+ As Cs Gs Ts))
       100.0)))

(defun max-gc-content (data)
  "Given: At most 10 DNA strings, `STRANDS', in FASTA format (of
length at most 1 kbp each).

Return: The ID of the string having the highest GC-content, followed
by the GC-content of that string. Rosalind allows for a default error
of 0.001 in all decimal answers unless otherwise stated; please see
the note on absolute error below."

  (let ((fasta-table (funs::make-fasta-hash-table data))
        (gc-table    (make-hash-table))
        (max-key     nil)
        (max-value   -1))
    (maphash #'(lambda (k v)
                 (setf (gethash k gc-table) (calc-gc-content v)))
             fasta-table)
    (with-hash-table-iterator (iter gc-table)
      (loop
         (multiple-value-bind (entry-p key value) (iter)
           (if entry-p
               (when (> value max-value)
                 (setf max-key   key)
                 (setf max-value value))
               (return)))))
    (list max-key (gethash max-key gc-table))))

(defun print-max-gc-content (data)
  (let ((max (max-gc-content data)))
    (format t "~a~%~6f" (car max) (cadr max))))

;;; Tests
(format t "
;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;
Calculate GC-Content: ~a
Max GC-Content: ~a
"
        (= (calc-gc-content "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT")
           *sample-gc-content-value*)
        (equal (max-gc-content *sample-dataset*) '("Rosalind_0808" 60.91954)))
