;;;; uniprot.lisp

(in-package #:uniprot)

(defun parse-fasta (string)
  (let* ((eol (position #\Newline string))
         (key (subseq string 0 eol))
         (data (remove #\Newline (subseq string
                                         (1+ eol)
                                         (length string)))))
    ;; TODO: split key into more details
    ;; (values key data)
    data))

(defun request-fasta (id)
  "Takes a protein `ID' to request its FASTA string from UniProt."
  (multiple-value-bind (body status)
      (drakma:http-request (format nil "http://www.uniprot.org/uniprot/~A.fasta" id))
    (when (= 200 status)
      (uniprot/parse-fasta body))))
