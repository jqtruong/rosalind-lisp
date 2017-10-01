;;;; funs.lisp

(in-package #:funs)

(defun to-string (nts)
  "Convert a list of characters representing nucleotides in `NTS' into a string."
  (format nil "~{~a~^~}" nts))

(defmacro memoize (name args desc body)
  "From http://kaygun.tumblr.com/post/98251739694/a-memoization-macro-for-common-lisp"
  (let ((hash-name (gensym)))
    `(let ((,hash-name (make-hash-table :test 'equal)))
       (defun ,name ,args
         ,desc
         (multiple-value-bind (val found-p) 
             (gethash (list ,@args) ,hash-name)
           (if found-p
               val
               (setf (gethash (list ,@args) ,hash-name)
                     ,body)))))))
