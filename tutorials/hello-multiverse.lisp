;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD-TUTORIALS; Base: 10 -*- file: hello-multiverse.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :silver-sword-tutorials)

(defun hello-multiverse ()
    (let ((solver "c4-sw_sample") ;; set up default solver
          (h (make-list 128 :initial-element 0)) ;; list of energies, h
          (j (make-hash-table)) ;; hash-table for couplers, J
          (answer nil)) ;; define answer symbol for later
      ;; Define the energy program
      ; energies for 'h'
      (setf (elt h 48) 0.5)
      (setf (elt h 53) 0.5)
      (setf (elt h 52) -0.5)
      (setf (elt h 49) -0.1)
      ; couplers for 'J'
      (setf (gethash "(48,53)" j) -0.5)
      (setf (gethash "(48,52)" j) 0.2)
      (setf (gethash "(52,49)" j) -0.3)
      (setf (gethash "(53,49)" j) 0.8)
      ;; Solve the problem
      (setf answer (ss:solve-ising solver h j :num-reads 100 :max-answers 100))
      ;; Print the results
      (ss::print-hash-table answer)
      ;; Or access them individually
      (format t "~%Solutions: ~S" (gethash "solutions" answer))
      (format t "~%128-bit Best solution: ~S" (elt (gethash "solutions" answer) 0))
      (format t "~%Lowest Energy: ~S" (elt (gethash "energies" answer) 0))
      (format t "~%Number of solutions: ~S" (length (gethash "solutions" answer)))
      (format t "~%Best solution: ~S ~S ~S ~S"
        (elt (elt (gethash "solutions" answer) 0) 56)
        (elt (elt (gethash "solutions" answer) 0) 60)
        (elt (elt (gethash "solutions" answer) 0) 57)
        (elt (elt (gethash "solutions" answer) 0) 61))))

;; after (ql:quickload "silver-sword-tutorials"), run (sst::hello-multiverse)
;; and watch it go!  run it a few times to compare your answers

;; EOF
