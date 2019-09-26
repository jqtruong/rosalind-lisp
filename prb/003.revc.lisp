;;;; revc.lisp

(defpackage :003.revc
  (:use :cl)
  (:import-from #:strand
                #:reverse-complement)
  (:export #:run
           #:*sample-dataset*
           #:*sample-output*
           #:*problem-description*))

(in-package #:003.revc)

(defvar *sample-dataset* "AAAACCCGGT")

(defun run (&optional (strand *sample-dataset*))
  "Given: A DNA string `STRAND' of length at most 1000 bp.

Return: The reverse complement of `STRAND'.

Sample Output:
ACCGGGTTTT"

  (reverse-complement strand))
