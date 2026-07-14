(defpackage :test-runner
  (:use :common-lisp)
  (:export #:run-all-tests))

(in-package :test-runner)

(defun run-all-tests ()
  (sparql-parser-test-scenario-a::run-tests)
  (sparql-parser-test-scenario-b::run-tests))
