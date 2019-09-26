;;;; strand.lisp

(defpackage :strand
  (:use :cl)
  (:export #:count-bases
           #:print-base-count
           #:transcribe))

(in-package #:strand)

(defun count-bases (strand)
  (loop for nt across strand
     counting (char-equal #\A nt) into Adenine
     counting (char-equal #\C nt) into Cytosine
     counting (char-equal #\G nt) into Guanine
     counting (char-equal #\T nt) into Thymine
     finally
       (return `((#\A . ,Adenine) (#\C . ,Cytosine) (#\G . ,Guanine) (#\T . ,Thymine)))))

(defun get-count (key bases)
  (cdr (assoc key bases)))

(defun get-majority-base (base-counts)
  (reduce #'(lambda (a b) 
              (cond ((> (cdr a) (cdr b)) a)
                    (t b)))
          base-counts))

(defun print-base-count (strand)
  (let ((bases (count-bases strand)))
    (format t "~d ~d ~d ~d"
            (get-count #\A bases)
            (get-count #\C bases)
            (get-count #\G bases)
            (get-count #\T bases))))

(defun reverse-complement (strand)
  "Loop through the reverse of the strand and collect the bases. Then, format the list of characters into a string."
  (let ((revc (loop for nt across (reverse strand)
                    collect (cond ((char-equal #\A nt) #\T)
                                  ((char-equal #\C nt) #\G)
                                  ((char-equal #\G nt) #\C)
                                  ((char-equal #\T nt) #\A)
                                  (t #\Space )))))
    (format t "~{~c~^~}"revc)))

(defun transcribe (strand)
  (substitute #\U #\T (string-upcase strand)))
