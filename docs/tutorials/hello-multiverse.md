SILVER-SWORD
============

Tutorial: Hello, Multiverse!
----------------------------

From the SBCL REPL, load `SILVER-SWORD` and the tutorials with Quicklisp:

```lisp
* (ql:quickload '("silver-sword" "silver-sword-tutorials"))
```

Run the `hello-multiverse` wrapper function:

```lisp
* (sst::hello-multiverse)
```

You should see a result similar to this:

```lisp
;; For #<HASH-TABLE :TEST EQUAL :COUNT 3 {100747FCE3}>
;; -- "energies": #(-3.4000000000000004 -1.4000000000000004)
;; -- "num_occurrences": #(99 1)
;; -- "solutions": #(#(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 -1 1 3 3 1 -1 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
                     #(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 -1 1 3 3 -1 -1 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                       3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))
Solutions: #(#(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
               3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 -1 1 3 3 1 -1 3 3 3 3 3 3 3 3 3
               3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
               3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
               3)
             #(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
               3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 -1 1 3 3 -1 -1 3 3 3 3 3 3 3 3 3
               3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
               3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
               3))
128-bit Best solution: #(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                         3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 -1 1 3 3 1
                         -1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                         3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
                         3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
Lowest Energy: -3.4000000000000004
Number of solutions: 2
Best solution: -1 1 1 -1
NIL
```

You may get back a different number of solutions, but the *best*---the solution that requires the lowest energy---should be the same.

Now let's take a look at the source code:

```lisp
(in-package :silver-sword-tutorials)

(defun hello-multiverse ()
    (let ((solver "c4-sw_sample") ;; set up default solver
          (h (make-list 128 :initial-element 0)) ;; list of energies, h
          (j (make-hash-table)) ;; hash-table for couplers, J
          (answer nil)) ;; define answer symbol for later
      ;; Define the energy program
      ; energies for 'h'
      (setf (elt h 48) 0.5
            (elt h 53) 0.5
            (elt h 52) -0.5
            (elt h 49) -0.1)
      ; couplers for 'J'
      (setf (gethash "(48,53)" j) -0.5
            (gethash "(48,52)" j) 0.2
            (gethash "(52,49)" j) -0.3
            (gethash "(53,49)" j) 0.8)
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
        (elt (elt (gethash "solutions" answer) 0) 48)
        (elt (elt (gethash "solutions" answer) 0) 49)
        (elt (elt (gethash "solutions" answer) 0) 52)
        (elt (elt (gethash "solutions" answer) 0) 53))))
```

Note that the `SOLVE-ISING` function expects the name of a local solver as a string, the qubit energies as a list, the couplers to energies as a hash-table, and a number of optional keyword arguments.  It returns a hash-table with the slots *solutions*, *energies*, and *num_occurences*.
