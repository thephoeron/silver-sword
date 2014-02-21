;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD-TUTORIALS; Base: 10 -*- file: spin-physics-local.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :silver-sword-tutorials)

(defun spin-physics ()
  (let ((solver "c4-sw_sample") ;; set up default solver
        (h (make-list 128 :initial-element 0)) ;; list of energies, h
        (j (make-hash-table)) ;; hash-table for couplers, J
        (spins (list 48 49 50 51 52 53 54 55))
        (couplings (list "(48,52)" "(52,49)" "(49,53)" "(53,50)" 
                         "(50,54)" "(54,51)" "(51,55)"))
        (fm-answer nil)
        (afm-answer nil)
        (dw-answer nil)
        (pdw-answer nil)
        (pt-answer nil))
    ;; Experiment 1: Building a ferromagnetic spin chain
    (loop for spin in spins
          do (setf (elt h spin) 0))
    (loop for coupling in couplings
          do (setf (gethash coupling j) -1))
    (setf fm-answer (ss:solve-ising solver h j :num-reads 1 :max-answers 100))
    (format t "~%;; Ferromagnetic chain:~
               ~%;; 48 = ~S~
               ~%;; 52 = ~S~
               ~%;; 49 = ~S~
               ~%;; 53 = ~S~
               ~%;; 50 = ~S~
               ~%;; 54 = ~S~
               ~%;; 51 = ~S~
               ~%;; 55 = ~S"
      (elt (elt (gethash "solutions" fm-answer) 0) 48)
      (elt (elt (gethash "solutions" fm-answer) 0) 52)
      (elt (elt (gethash "solutions" fm-answer) 0) 49)
      (elt (elt (gethash "solutions" fm-answer) 0) 53)
      (elt (elt (gethash "solutions" fm-answer) 0) 50)
      (elt (elt (gethash "solutions" fm-answer) 0) 54)
      (elt (elt (gethash "solutions" fm-answer) 0) 51)
      (elt (elt (gethash "solutions" fm-answer) 0) 55))))

;; EOF
