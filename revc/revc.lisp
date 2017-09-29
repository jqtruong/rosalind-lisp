;;;; revc.lisp

(in-package #:revc)

(defvar *sample* "AAAACCCGGT")

(defun reverse-complement (strand)
  "Given: A DNA string `STRAND' of length at most 1000 bp.

Return: The reverse complement of `STRAND'.

Sample Output:
ACCGGGTTTT"

  (loop for nt across (reverse strand)
        collect (cond ((char-equal #\A nt) #\T)
                      ((char-equal #\C nt) #\G)
                      ((char-equal #\G nt) #\C)
                      ((char-equal #\T nt) #\A)
                      (t #\Space ))))
