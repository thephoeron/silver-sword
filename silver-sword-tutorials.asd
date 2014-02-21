;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD-TUTORIALS; Base: 10 -*- file: silver-sword-tutorials.asd

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(defsystem :silver-sword-tutorials
  :version #.silver-sword-asd:*silver-sword-version*
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :description "Tutorials for SILVER-SWORD: Common Lisp interface to D-Wave Python Pack, adapted from original D-Wave developer portal tutorials."
  :license "MIT"
  :serial t
  :depends-on (:burgled-batteries
               :gsll
               :cl-plplot
               :silver-sword)
  :pathname #P"tutorials/"
  :components ((:file "packages")
               (:file "hello-multiverse")
               (:file "hadamard-blackbox")
               (:file "nand-gate-local")
               (:file "spin-physics-local")))

;; EOF
