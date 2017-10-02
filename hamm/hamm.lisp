;;;; hamm.lisp

(in-package #:hamm)

(defvar *sample-dataset-1* "GAGCCTACTAACGGGAT")
(defvar *sample-dataset-2* "CATCGTAATGACGGCCT")

(defvar *sample-output* 7)

(defun distance (s1 s2)
  "Given: Two DNA strings `S1' and `S2' of equal length (not exceeding 1 kbp).

Return: The Hamming distance."

  (loop for c1 across s1
        for c2 across s2
        count (not (char-equal c1 c2)) into distance
        finally (return distance)))
