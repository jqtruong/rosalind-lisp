;;;; iev.lisp

(in-package #:iev)

(defparameter *genotype-pairings* '((2 0 0)   ; AA-AA
                                    (1 1 0)   ; AA-Aa
                                    (1 0 1)   ; AA-aa
                                    (0 2 0)   ; Aa-Aa
                                    (0 1 1)   ; Aa-aa
                                    (0 0 2))) ; aa-aa

(defvar *sample-dataset* '(1 0 0 1 0 1))
(defvar *sample-output*  3.5)

(defun dominance-expected-value (genotypical-couples children-per-couple)
  "Given: Six nonnegative integers in `GENOTYPICAL-COUPLES', each of which does
not exceed 20,000. The integers correspond to the number of couples in a
population possessing each genotype pairing for a given factor. In order, the
six given integers represent the number of couples having the following
genotypes: (see `*GENOTYPE-PAIRINGS*').

Return: The expected number of offspring displaying the dominant phenotype in
the next generation, under the assumption that every couple has exactly
`CHILDREN-PER-COUPLE' offspring."

  (loop for num in genotypical-couples
     for pairing in *genotype-pairings*
     collect (* num (apply #'iprb::dominance-ratio pairing)) into expected-values
     finally
       (return (* children-per-couple (apply #'+ expected-values)))))
