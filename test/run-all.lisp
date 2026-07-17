(defpackage :test-runner
  (:use :common-lisp)
  (:export #:run-all-tests))

(in-package :test-runner)

(defun run-all-tests ()
  (sparql-parser-test-integration::run-tests))
