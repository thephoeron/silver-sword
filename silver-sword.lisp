;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SILVER-SWORD; Base: 10 -*- file: silver-sword.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :silver-sword)

;; Load Python environment and D-Wave SAPI
(eval-when (:compile-toplevel :execute :load-toplevel)
    (burgled-batteries:startup-python)
    (burgled-batteries:run "from dwave_sapi import *"))

;; HASH-TABLE Utility functions
(defun print-hash-entry (key value)
    (format t ";; -- ~S: ~S~%" key value))

(defun print-hash-table (the-hash-table)
    "Map over hash-table object and print 'key: value' pairs."
    (format t "~%;; For ~S~%" the-hash-table)
    (maphash #'print-hash-entry the-hash-table))

;; Throw-away function, useful for checking to see how BURGLED-BATTERIES mangles python names
(defun list-all-symbols ()
    (loop for s being the symbols of (find-package "SILVER-SWORD")
      collect s))

;; LOCAL_CONNECTION.SOLVER_NAMES
(burgled-batteries:defpyfun "local_connection.solver_names" ())

(defvar *local-solver-names-python-documentation*
    (documentation 'local_connection.solver_names 'function))

(defun local-solver-names ()
    "Return list of all available local solver names for local-get-solver."
    (local_connection.solver_names))

;; LOCAL_CONNECTION.GET_SOLVER
(burgled-batteries:defpyfun "local_connection.get_solver" (name))

(defvar *local-get-solver-python-documentation*
    (documentation 'local_connection.get_solver 'function))

(defun local-get-solver (name)
    "Get local solver by name."
    ;(local_connection.get_solver name)
    (burgled-batteries:run (format nil "solver = local_connection.get_solver(\"~A\")" name))
    (burgled-batteries:run "solver"))

(defun local-solver-qubits (solver-name)
    "List the current working qubits available on the passed solver."
    (local-get-solver solver-name)
    (burgled-batteries:run "solver.qubits"))

(defun local-solver-total-qubits (solver-name)
    "Return the total number of qubits on the passed solver."
    (local-get-solver solver-name)
    (burgled-batteries:run "solver.num_qubits"))

(defun local-solver-couplers (solver-name)
    "List the couplings of working and available qubits on the passed solver."
    (local-get-solver solver-name)
    (burgled-batteries:run "solver.couplers"))

;; SOLVE_ISING
(burgled-batteries:defpyfun "solve_ising" (solver-name h j &key num-reads num-programming-cycles answer-mode max-answers))

(defvar *solve-ising-python-documentation*
    (documentation 'solve_ising 'function))

(defun solve-ising (solver-name h j &key (num-reads 1) (num-programming-cycles 1) (answer-mode "histogram") (max-answers 1))
    "Solve Ising using designated solver. h must be a list of energies, j a hash-table of couplings to energies. Returns a hash-table with the keys: solutions, energies, num_occurences."
    (let* ((the-solver (format nil "local_connection.get_solver(\"~A\")" solver-name))
           (the-energies (format nil "[~{~A~^, ~}]" h))
           (the-couplings (format nil "{~{~a~^, ~}}"
             (loop for key being the hash-keys of j
                   using (hash-value value)
                   collect (format nil "~A: ~A" key value))))
           (the-params (make-hash-table))
           (solve-string "")
           (param-string ""))
      (setf (gethash 'num_reads the-params) num-reads)
      (setf (gethash 'num_programming_cycles the-params) num-programming-cycles)
      (setf (gethash 'answer_mode the-params) answer-mode)
      (setf (gethash 'max_answers the-params) max-answers)
      (setf param-string (format nil "{~{~A~^, ~}}"
        (loop for key being the hash-keys of the-params
              using (hash-value value)
              collect (format nil "~S: ~S" (string-downcase key) value))))
      (setf solve-string (format nil "solve_ising(~A,~A,~A,**~A)" the-solver the-energies the-couplings param-string))
      (burgled-batteries:run solve-string)))

;; SOLVE_QUBO
(burgled-batteries:defpyfun "solve_qubo" (solver-name q &key num-reads num-programming-cycles answer-mode max-answers))

(defvar *solve-qubo-python-documentation*
    (documentation 'solve_qubo 'function))

(defun solve-qubo (solver-name q &key (num-reads 1) (num-programming-cycles 1) (answer-mode "histogram") (max-answers 1))
    "Solve Qubo using designated solver. q must be a hash-table of couplings to energies. Returns a hash-table with the keys: solutions, energies, num_occurences."
    (let* ((the-solver (format nil "local_connection.get_solver(\"~A\")" solver-name))
           (the-couplings (format nil "{~{~a~^, ~}}"
             (loop for key being the hash-keys of q
                   using (hash-value value)
                   collect (format nil "~A: ~A" key value))))
           (the-params (make-hash-table))
           (solve-string "")
           (param-string ""))
      (setf (gethash 'num_reads the-params) num-reads)
      (setf (gethash 'num_programming_cycles the-params) num-programming-cycles)
      (setf (gethash 'answer_mode the-params) answer-mode)
      (setf (gethash 'max_answers the-params) max-answers)
      (setf param-string (format nil "{~{~A~^, ~}}"
        (loop for key being the hash-keys of the-params
              using (hash-value value)
              collect (format nil "~S: ~S" (string-downcase key) value))))
      (setf solve-string (format nil "solve_qubo(~A,~A,**~A)" the-solver the-couplings param-string))
      (burgled-batteries:run solve-string)))

;; WRITE-QUBO
(burgled-batteries:defpyfun "write_qubo" ())

(defvar *write-qubo-python-documentation*
    (documentation 'write_qubo 'function))

;; READ-QUBO
(burgled-batteries:defpyfun "read_qubo" ())

(defvar *read-qubo-python-documentation*
    (documentation 'read_qubo 'function))

;; ISING-TO-QUBO
(burgled-batteries:defpyfun "ising_to_qubo" ())

(defvar *ising-to-qubo-python-documentation*
    (documentation 'ising_to_qubo 'function))

;; QUBO-TO-ISING
(burgled-batteries:defpyfun "qubo_to_ising" ())

(defvar *qubo-to-ising-python-documentation*
    (documentation 'qubo_to_ising 'function))

;; GET-CHIMERA-ADJACENCY
(burgled-batteries:defpyfun "get_chimera_adjacency" ())

(defvar *get-chimera-adjacency-python-documentation*
    (documentation 'get_chimera_adjacency 'function))

;; GET-HARDWARE-ADJACENCY
(burgled-batteries:defpyfun "get_hardware_adjacency" ())

(defvar *get-hardware-adjacency-python-documentation*
    (documentation 'get_hardware_adjacency 'function))

;; LINEAR-INDEX-TO-CHIMERA
(burgled-batteries:defpyfun "linear_index_to_chimera" ())

(defvar *linear-index-to-chimera-python-documentation*
    (documentation 'linear_index_to_chimera 'function))

;; CHIMERA-TO-LINEAR-INDEX
(burgled-batteries:defpyfun "chimera_to_linear_index" ())

(defvar *chimera-to-linear-index-python-documentation*
    (documentation 'chimera_to_linear_index 'function))

;; FIX-VARIABLES-SOLVER
(burgled-batteries:defpyfun "FixVariablesSolver" ())

(defvar *fix-variables-solver-python-documentation*
    (documentation 'fixvariablessolver 'function))

;; EMBEDDING-SOLVER
(burgled-batteries:defpyfun "EmbeddingSolver" ())

(defvar *embedding-solver-python-documentation*
    (documentation 'embeddingsolver 'function))

;; BLACK-BOX-SOLVER
(burgled-batteries:defpyfun "BlackBoxSolver" ())

(defvar *black-box-solver-python-documentation*
    (documentation 'blackboxsolver 'function))

(burgled-batteries:defpyfun "BlackBoxSolver.solve" ())

(defvar *black-box-solver-solve-python-documentation*
    (documentation 'blackboxsolver.solve 'function))

;; REDUCE-DEGREE
(burgled-batteries:defpyfun "reduce_degree" ())

(defvar *reduce-degree-python-documentation*
    (documentation 'reduce_degree 'function))

;; MAKE-QUADRATIC
(burgled-batteries:defpyfun "make_quadratic" ())

(defvar *make-quadratic-python-documentation*
    (documentation 'make_quadratic 'function))

;; EOF
