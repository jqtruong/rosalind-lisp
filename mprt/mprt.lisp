;;;; mprt.lisp
;;;; Motif Protein

(in-package #:mprt)

(defvar *sample-dataset* '(A2Z669
                           B5ZC00
                           P07204_TRBM_HUMAN
                           P20840_SAG1_YEAST))

(defvar *sample-output* "B5ZC00
85 118 142 306 395
P07204_TRBM_HUMAN
47 115 116 382 409
P20840_SAG1_YEAST
79 109 135 248 306 348 364 402 485 501 614")

(defvar *N-glycosylation* "N{P}[ST]{P}")

(defun create-motif-scanner (motif)
  "Takes a motif expression and converts it to a regular expression
  with CL-PPCRE."

  (let ((regexp motif))
    (cl-ppcre::create-scanner regexp)))

(defun search-fasta (fasta motif)
  fasta)

(defun problem (uniprot-ids)
  "Given: At most 15 UniProt Protein Database access IDs.

  Return: For each protein possessing the N-glycosylation motif,
  output its given access ID followed by a list of locations in the
  protein string where the motif can be found."

  (loop for id in uniprot-ids
        for fasta = (funs::uniprot/request-fasta id)
        collect (cons id (search-fasta fasta *N-glycosylation*))))
