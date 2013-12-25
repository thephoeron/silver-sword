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
