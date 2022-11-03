(in-package :cl-user)

(defpackage :tree-db
  (:use :common-lisp)
  (:shadow :equal)
  (:export #:create #:val #:keys-at #:all-keys #:equal #:copy))

(defpackage #:sparql-parser
  (:use :common-lisp))

(defpackage :sparql-bnf)
