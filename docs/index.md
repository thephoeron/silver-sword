SILVER-SWORD
============

Common Lisp interface to D-Wave's Python Pack for adiabatic quantum computer energy programming, including translations of tutorials into CL.

*Note*: with the launch of the new D-Wave Systems website, the Developer Portal and downloads of the Python Pack for registered developers is no longer available.

For more information on D-Wave Systems and adiabatic quantum computer energy programming, please see the [D-Wave Resources](http://www.dwavesys.com/resources).

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
* PL-Plot 5.10+
* SBCL 1.1.14+
* Quicklisp

Dependencies
------------

From Quicklisp:

* burgled-batteries
* gsll
* cl-plplot

Documentation
-------------

* [API Documentation](api/)
* [Tutorials](tutorials/index/)
* [Source Code](https://github.com/thephoeron/silver-sword)
