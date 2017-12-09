;;;; lia.lisp

(in-package #:lia)

(defvar *sample-dataset* '(2 1))

(defvar *sample-output* 0.684)

(defun at-least (PR_n-1 n &optional (sum 0))
  (let ((pr (apply PR_n-1 `(,n))))
    (cond ((= n 0) (+ sum pr))
          (t (at-least PR_n-1
                       (1- n)
                       (+ sum pr))))))

(defun problem (k N)
  "Given `K' generations of a family tree that reproduces 2 children
  each time, starting with `AaBb' parents and partnering only with
  `AaBb' mates, calculate the probablity that at least `N' children
  will also be `AaBb'. In this gene pool, the probability that no
  children gets those alleles is 3/4 (see README.org)."

  (let* ((children (expt 2 k))
         (children! (funs::! children)))
    (- 1 (at-least (lambda (x)
                     (/ (* children!
                           (expt 1/4 x)
                           (expt 3/4 (- children x)))
                        (* (funs::! x) (funs::! (- children x)))))
                   (1- N)))))
