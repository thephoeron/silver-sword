;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD-TUTORIALS; Base: 10 -*- file: nand-gate-local.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :silver-sword-tutorials)

(defun nand-gate ()
    (let ((solver "c4-sw_sample") ;; set up default solver
          (h (make-list 128 :initial-element 0)) ;; list of energies, h
          (j (make-hash-table)) ;; hash-table for couplers, J
          (answer nil)) ;; define answer symbol for later
      ;; Define the energy program
      ; energies for 'h'
      (setf (elt h 48) -0.1
            (elt h 49) -0.1
            (elt h 53) -0.2)
      ; couplers for 'J'
      (setf (gethash "(48,53)" j) 0.2
            (gethash "(48,52)" j) 0.1
            (gethash "(49,52)" j) -1
            (gethash "(49,53)" j) 0.2)
      ;; Solve the problem
      (setf answer (ss:solve-ising solver h j :num-reads 100 :max-answers 100))
      ;; Print the individual qubit states
      (format t "~%;; Qubit states:~%;; 48 = ~S~%;; 49 = ~S~%;; 52 = ~S~%;; 53 = ~S"
        (elt (elt (gethash "solutions" answer) 0) 48)
        (elt (elt (gethash "solutions" answer) 0) 49)
        (elt (elt (gethash "solutions" answer) 0) 52)
        (elt (elt (gethash "solutions" answer) 0) 53))
      ;; Now look at the solutions, energies, and compare to correct answers
      (ss::print-hash-table answer)
      (format t "~%;; Best solution: ~S" (elt (gethash "solutions" answer) 0))
      (format t "~%;; Lowest Energy: ~S" (elt (gethash "energies" answer) 0))
      (format t "~%;; Number of solutions: ~S" (length (gethash "solutions" answer)))
      (format t "~%;; Sampled solution: ~S ~S ~S ~S~%"
        (elt (elt (gethash "solutions" answer) 0) 48)
        (elt (elt (gethash "solutions" answer) 0) 52)
        (elt (elt (gethash "solutions" answer) 0) 49)
        (elt (elt (gethash "solutions" answer) 0) 53))
      (format t "~%;; Correct answers:~%;; -1 1 1 1~%;; -1 -1 -1 1~%;; 1 -1 -1 1~%;; 1 1 1 -1")))

;; after (ql:quickload "silver-sword-tutorials"), run (sst::nand-gate)
;; and watch it go!  run it a few times to compare your answers

;; EOF
