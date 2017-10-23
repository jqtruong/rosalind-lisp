;;;; iprb.lisp

(in-package #:iprb)

(defvar *sample-dataset* '(2 2 2))
(defvar *sample-output* 0.78333)

(defun dominance-ratio (k m n)
  "Given: Three positive integers `K', `M', and `N',
representing a population containing $k+m+n$ organisms: `K' individuals are
homozygous dominant for a factor, `M' are heterozygous, and `N' are homozygous
recessive.

Return: The probability that two randomly selected mating organisms will produce
an individual possessing a dominant allele (and thus displaying the dominant
phenotype). Assume that any two organisms can mate."

  (let* ((ini (+ k m n))                ; initial population
         (rem (1- ini)))                ; remaining population
    (+
     ;; 1st: k, 2nd: k; 100%
     (* (/ k ini)
        (/ (1- k) rem))
     ;; 1st: k, 2nd: m; 100%
     (* (/ k ini)
        (/ m rem))
     ;; 1st: k, 2nd: n; 100%
     (* (/ k ini)
        (/ n rem))
     ;; 1st: m, 2nd: k; 100%
     (* (/ m ini)
        (/ k rem))
     ;; 1st: m, 2nd: m; 75%
     (* (/ m ini)
        (/ (1- m) rem)
        .75)
     ;; 1st: m, 2nd: n; 50%
     (* (/ m ini)
        (/ n rem)
        .5)
     ;; 1st: n, 2nd: k; 100%
     (* (/ n ini)
        (/ k rem))
     ;; 1st: n, 2nd: m; 50%
     (* (/ m ini)
        (/ n rem)
        .5))))

;;; Tests
(format t "
;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;
Calculate dominance ratio: ~a
"
        (= (float (/ (truncate (* 100000 (apply #'dominance-ratio *sample-dataset*))) 100000))
           *sample-output*))
