;;;; funs.lisp

(in-package #:funs)

(defun to-string (chars)
  "Convert a list of characters in `CHARS' into a string."
  (format nil "~{~a~^~}" chars))

(defun join-list (list sep)
  (loop for item in list
     do (format t "~a" item)
     while (not (equal item (car (last list))))
     do (format t "~a" sep)))

(defmacro memoize (name args desc body)
  "From http://kaygun.tumblr.com/post/98251739694/a-memoization-macro-for-common-lisp."
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

(defun hash-keys (table)
  (loop for key being the hash-keys in table collect key))

;;; 
;;;--- TODO move to fasta project
;;; 
;;;--- TODO add safety check for newline at the end, or a real
;;;--- solution!
;;; 
(defun make-fasta-hash-table (data)
  "Make a hash table of FASTA strings from `DATA'."
  (let ((fasta-table (make-hash-table :test 'equal))
        (data-len (length data)))
    (loop 
       for i = 0 then l

       while (not (= i data-len))
       for j = (position #\> data :start i) ; start of a FASTA string
       for k = (position #\Newline data :start j) ; end of label
       for l = (or (position #\> data :start k)   ; end of strand
                   data-len)
       for key = (subseq data (1+ j) k)
       for val = (subseq data (1+ k) (1- l))
       do (setf (gethash key fasta-table) (remove #\Newline val)))

    fasta-table))

(defun ! (x &optional (acc 1))
  "Factorial!"
  (cond ((= x 1) acc)
        ((> x 1) (! (1- x) (* acc x)))
        (t acc)))

(defun parse-prot-fasta (string)
  (let* ((eol (position #\Newline string))
         (key (subseq string 0 eol))
         (data (remove #\Newline (subseq string (1+ eol) (length string)))))
    ;; TODO: split key into more details
    ;; (values key data)
    data))

(defun uniprot/request-fasta (id)
  "Takes a protein `ID' to request its FASTA string from UniProt."
  (multiple-value-bind (body status)
      (drakma:http-request (format nil "http://www.uniprot.org/uniprot/~A.fasta" id))
    (when (= 200 status)
      (parse-prot-fasta body))))
