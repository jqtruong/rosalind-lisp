;;;; prot.lisp

(in-package #:prot)

(defvar *sample-dataset* "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
(defvar *sample-output*  "MAMAPRTEINSTRING")

(defvar *codon-table*
  (funs::construct-table '(UUU F      CUU L      AUU I      GUU V
                           UUC F      CUC L      AUC I      GUC V
                           UUA L      CUA L      AUA I      GUA V
                           UUG L      CUG L      AUG M      GUG V
                           UCU S      CCU P      ACU T      GCU A
                           UCC S      CCC P      ACC T      GCC A
                           UCA S      CCA P      ACA T      GCA A
                           UCG S      CCG P      ACG T      GCG A
                           UAU Y      CAU H      AAU N      GAU D
                           UAC Y      CAC H      AAC N      GAC D
                           UAA Stop   CAA Q      AAA K      GAA E
                           UAG Stop   CAG Q      AAG K      GAG E
                           UGU C      CGU R      AGU S      GGU G
                           UGC C      CGC R      AGC S      GGC G
                           UGA Stop   CGA R      AGA R      GGA G
                           UGG W      CGG R      AGG R      GGG G)))

(defun codon->acid (codon)
  (cdr (assoc codon *codon-table* :test #'string=)))

(defun codons->acids (strand)
  "Given: An RNA string `STRAND' corresponding to a strand of mRNA (of length at most 10 kbp).

Return: The protein string encoded by `STRAND'."

  (let ((len (length strand)))
    (loop
       for i = 0 then (+ i 3)
       while (< i len)
       for acid = (codon->acid (subseq strand i (+ i 3)))
       until (string= acid "STOP")
       collect acid)))
