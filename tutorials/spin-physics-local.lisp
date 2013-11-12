;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD-TUTORIALS; Base: 10 -*- file: spin-physics-local.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :silver-sword-tutorials)

;; CL-PLPLOT is failing miserably, so we'll have to call mplotlib
;; through burgled-batteries after the original tutorial

(defun spin-physics ()
  (let ((solver "c4-sw_sample") ;; set up default solver
        (h (make-list 128 :initial-element 0)) ;; list of energies, h
        (j (make-hash-table)) ;; hash-table for couplers, J
        (fm-answer nil)
        (afm-answer nil)
        (dw-answer nil)
        (pdw-answer nil)
        (pt-answer nil))))

;; EOF
