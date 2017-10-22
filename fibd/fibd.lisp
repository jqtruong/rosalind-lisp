;;;; fibd.lisp

(in-package #:fibd)

(funs::memoize
 calc-rabbits (n m)
 "Given: Positive integers n ≤ 100 and m ≤ 20.

Return: The total number of pairs of rabbits that will remain after
the n-th month if all rabbits live for m months."

 (cond
   ((zerop n) 0)
   ((= n 1)   1)
   ((= n 2)   1)
   ((<= n m)  (+ (calc-rabbits (- n 1) m)   ; number of pairs in the last cycle
                 (calc-rabbits (- n 2) m))) ; number of pairs 2 cycles ago
   (t (apply #'+ (loop for x from m downto 2
                    collect (calc-rabbits (- n x) m))))))
