;;;; subs.lisp

(in-package #:subs)

(defvar *sample-dataset-source* "GATATATGCATATACTT")
(defvar *sample-dataset-repeat* "ATAT")
(defvar *sample-output* '(2 4 10))

(defun collect-repeats-indices (source repeat)
  "Given: Two DNA strings `SOURCE' and `REPEAT' (each of length at most 1 kbp).

Return: All locations of `REPEAT' as a substring of `SOURCE'."

  (loop
     for i = 0 then (1+ i)
     for j = (+ i (length repeat))
     while (< j (length source))

     for r = (subseq source i j)
     when (string= r repeat)
     collect (1+ i)))

;;; Tests
(format t "
;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;
Indices of repeated nucleotides: ~a
"
        (equal *sample-output*
               (collect-repeats-indices *sample-dataset-source* *sample-dataset-repeat*)))
