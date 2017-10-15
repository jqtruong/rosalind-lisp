;;;; funs.lisp

(in-package #:funs)

(defun to-string (chars)
  "Convert a list of characters in `CHARS' into a string."
  (format nil "~{~a~^~}" chars))

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

(defmacro construct-table (copy)
  "Raw copied table, e.g. "
  `(loop for (k v) on ,copy by #'cddr
      for key = (if (symbolp k)
                    (symbol-name k)
                    k)
      for val = (if (symbolp v)
                    (symbol-name v)
                    v)
      collect (cons key val)))
