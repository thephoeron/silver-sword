# SILVER-SWORD

[![Not Available in Quicklisp](https://img.shields.io/badge/quicklisp-not%20available-red.svg)](https://quicklisp.org/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Common Lisp interface to D-Wave's Python Pack for adiabatic quantum computer energy programming, including translations of tutorials into CL.

Documentation
-------------

Documentation and Tutorials available at: http://thephoeron.viewdocs.io/silver-sword

Usage
-----

A good place to start is with the tutorial `hello-multiverse.lisp`.  Take a moment to review the code and make note of the following points:

* The SOLVE-ISING function expects the name of a local solver as a string, the qubit energies as a list, the couplers to energies as a hash-table, and a number of optional keyword arguments.  It returns a hash-table with the slots `solutions`, `energies`, and `num_occurences`.
* Since the Python Pack is based on the architecture of the D-Wave One adiabatic quantum computer, a maximum of 128 qubits and 352 couplers is available.

Load SILVER-SWORD and its tutorial package with quicklisp:

```lisp
* (ql:quickload '("silver-sword" "silver-sword-tutorials"))
```

Run the `hello-multiverse` wrapper function:

```lisp
* (sst:hello-multiverse)
```

You should see a result similar to this:

```lisp
For #<HASH-TABLE :TEST EQUAL :COUNT 3 {100747FCE3}>
"energies": #(-3.4000000000000004 -1.4000000000000004)
"num_occurrences": #(99 1)
"solutions": #(#(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
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

System Requirements
-------------------

* Linux x86_64 or Windows XP, Vista, 7 (32/64 bit)
* Python 2.7.x
* D-Wave Python Pack 1.4.0
* PL-Plot 5.10+
* SBCL 1.1.14+
* Quicklisp

*Note*: with the launch of the new D-Wave Systems website, the Developer Portal and downloads of the Python Pack for registered developers is no longer available.

Dependencies
------------

From Quicklisp:

* burgled-batteries
* gsll (GNU Scientific Library for Lisp)
* cl-plplot
