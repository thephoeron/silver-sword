;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD; Base: 10 -*- file: silver-sword.asd

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)


(defpackage silver-sword-asd
    (:use :cl :asdf)
    (:export #:*silver-sword-version*))

(in-package :silver-sword-asd)

(defparameter *silver-sword-version* "0.1.1")

(defsystem silver-sword
    :version #.*silver-sword-version*
    :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
    :license "MIT"
    :description "Common Lisp interface to D-Wave's Python Pack for adiabatic quantum computer energy programming."
    :serial t
    :depends-on (:burgled-batteries)
    :components ((:file "packages")
                 (:file "silver-sword")))

;; EOF
