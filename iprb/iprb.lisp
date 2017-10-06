;;;; iprb.lisp

(in-package #:iprb)

(defvar *sample-dataset* '(2 2 2))
(defvar *sample-output* 0.78333)

(defun calc-dominance (k m n)
  "Given: Three positive integers `K', `M', and `N',
representing a population containing $k+m+n$ organisms: `K' individuals are
homozygous dominant for a factor, `M' are heterozygous, and `N' are homozygous
recessive.

Return: The probability that two randomly selected mating organisms will produce
an individual possessing a dominant allele (and thus displaying the dominant
phenotype). Assume that any two organisms can mate."

  (let* ((tot1 (+ k m n))
         (tot2 (1- tot1)))
    (+
     ;; 1st: k, 2nd: k; 100%
     (* (/ k tot1)
        (/ (1- k) tot2))
     ;; 1st: k, 2nd: m; 100%
     (* (/ k tot1)
        (/ m tot2))
     ;; 1st: k, 2nd: n; 100%
     (* (/ k tot1)
        (/ n tot2))
     ;; 1st: m, 2nd: k; 100%
     (* (/ m tot1)
        (/ k tot2))
     ;; 1st: m, 2nd: m; 75%
     (* (/ m tot1)
        (/ (1- m) tot2)
        .75)
     ;; 1st: m, 2nd: n; 50%
     (* (/ m tot1)
        (/ n tot2)
        .5)
     ;; 1st: n, 2nd: k; 100%
     (* (/ n tot1)
        (/ k tot2))
     ;; 1st: n, 2nd: m; 50%
     (* (/ m tot1)
        (/ n tot2)
        .5))))

;;; Tests
(format t "
;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;
Calculate dominance chance: ~a
"
        (= (float (/ (truncate (* 100000 (apply #'calc-dominance *sample-dataset*))) 100000))
           *sample-output*))
