;;;; fib.lisp

(in-package #:fib)

;;; "fib" goes here. Hacks and glory await!

(defvar *sample* '(5 3))

(funs::memoize
 rabbit-fib (n k)
 "Given: Positive integers `N' (cycle) and `K' (growth rate).

Return: The total number of rabbit pairs that will be present after
`N' months, if we begin with 1 pair and in each generation, every pair
of reproduction-age rabbits produces a litter of `K' rabbit
pairs (instead of only 1 pair).

Sample Output:
19

A key observation is that the number of offspring in any month is
equal to the number of rabbits that were alive two months prior"

 (cond
   ((zerop n) 0)
   ((= n 1)   1)
   (t (+ (rabbit-fib (1- n) k)     ; number of pairs in the last cycle
         (* (rabbit-fib (- n 2) k) ; number of pairs 2 cycles ago
            k)))))                 ; growth rate
