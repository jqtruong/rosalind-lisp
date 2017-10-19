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

   ;; need more reqs here depending on mortality rate
   
   (t (+ (rabbit-fib (1- n) k)     ; number of pairs in the last cycle
         (* (rabbit-fib (- n 2) k) ; number of pairs 2 cycles ago
            k)))))
