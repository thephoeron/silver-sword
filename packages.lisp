;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD; Base: 10 -*- file: packages.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:silver-sword
    (:nicknames #:ss)
    (:use :cl)
    (:export #:*silver-sword-version*
             #:local-solver-names #:*local-solver-names-python-documentation*
             #:local-get-solver #:*local-get-solver-python-documentation*
             #:local-solver-qubits #:local-solver-total-qubits
             #:local-solver-couplers
             #:solve-ising #:*solve-ising-python-documentation*
             #:solve-qubo #:*solve-qubo-python-documentation*))

;; see asdf system definition
(defvar silver-sword:*silver-sword-version*
  #.silver-sword-asd::*silver-sword-version*)

;; EOF
