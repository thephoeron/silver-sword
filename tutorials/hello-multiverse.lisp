;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD-TUTORIALS; Base: 10 -*- file: hello-multiverse.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :silver-sword-tutorials)

;; We set up the default sample local solver
(defparameter *hello-multiverse-solver* "c4-sw_sample")

;; Define objects to hold the energy program
(defparameter *hello-multiverse-h* (make-list 128 :initial-element 0))
(defparameter *hello-multiverse-j* (make-hash-table))

;; Define the energy program
(setf (elt *hello-multiverse-h* 48) 0.5)
(setf (elt *hello-multiverse-h* 53) 0.5)
(setf (elt *hello-multiverse-h* 52) -0.5)
(setf (elt *hello-multiverse-h* 49) -0.1)

(setf (gethash "(48,53)" *hello-multiverse-j*) -0.5)
(setf (gethash "(48,52)" *hello-multiverse-j*) 0.2)
(setf (gethash "(52,49)" *hello-multiverse-j*) -0.3)
(setf (gethash "(53,49)" *hello-multiverse-j*) 0.8)

(defun hello-multiverse ()
    ;; Solve the problem
    (defparameter *hello-multiverse-answer*
        (ss:solve-ising *hello-multiverse-solver*
                         *hello-multiverse-h*
                         *hello-multiverse-j*
                         :num-reads 100
                         :max-answers 100))
    ;; Print the results
    (ss::print-hash-table *hello-multiverse-answer*)
    ;; Or access them individually
    (format t "~%Solutions: ~S" (gethash "solutions" *hello-multiverse-answer*))
    (format t "~%128-bit Best solution: ~S" (elt (gethash "solutions" *hello-multiverse-answer*) 0))
    (format t "~%Lowest Energy: ~S" (elt (gethash "energies" *hello-multiverse-answer*) 0))
    (format t "~%Number of solutions: ~S" (length (gethash "solutions" *hello-multiverse-answer*)))
    (format t "~%Best solution: ~S ~S ~S ~S"
        (elt (elt (gethash "solutions" *hello-multiverse-answer*) 0) 56)
        (elt (elt (gethash "solutions" *hello-multiverse-answer*) 0) 60)
        (elt (elt (gethash "solutions" *hello-multiverse-answer*) 0) 57)
        (elt (elt (gethash "solutions" *hello-multiverse-answer*) 0) 61)))

;; after (ql:quickload "silver-sword-tutorials"), run (sst::hello-multiverse)
;; and watch it go!  run it a few times to compare your answers

;; EOF
