SILVER-SWORD
============

Common Lisp interface to D-Wave's Python Pack for adiabatic quantum computer energy programming, including translations of tutorials into CL.

*Note*: access to D-Wave's Python Pack for adiabatic quantum computer energy programming is limited to registered developers, and registration for the D-Wave Developer Portal is now closed.

For more information, please see [the D-Wave Developer Portal](http://www.dwavesys.com/en/dev-portal.html).

Getting Started
---------------

Make sure you have all system requirements and dependencies met, and then clone the Silver-Sword repo into `~/quicklisp/local-projects/`.

To get up and running, evaluate `(ql:quickload '("silver-sword" "silver-sword-tutorials"))` from the SBCL REPL, and follow along with the tutorials below.

It is worth noting that D-Wave's Python Pack v1.4.0 is based on the architecture of the D-Wave One adiabatic quantum computer, so a maximum of 128 qubits and 352 couplers is available in the Local Solver.

System Requirements
-------------------

* Linux x86_64 or Windows XP/Vista/7 (32/64bit)
* Python 2.7.x
* D-Wave Python Pack 1.4.0
* GNU Plot 4.6+
* SBCL 1.1.7+
* Quicklisp

Dependencies
------------

From Quicklisp:

* burgled-batteries
* gsll
* cgn
* ltk
* cl-fad

Documentation
-------------

* [API Documentation](api/)
* [Tutorials](tutorials/index/)
* [Source Code](https://github.com/thephoeron/silver-sword)
