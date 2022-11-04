(in-package :cl-user)

(defpackage :tree-db
  (:use :common-lisp)
  (:shadow :equal)
  (:export #:create #:val #:keys-at #:all-keys #:equal #:copy))

(defpackage :support
  (:use :common-lisp)
  (:export #:-> #:debug-break #:embed-unicode-characters #:hex-char))

(defpackage :sparql-bnf
  (:export #:|rule| #:|terminal| #:|first| #:|follow| #:|seq| #:|alt| #:|_eof| #:|_eps| #:|_empty|))

(defpackage :sparql-terminals
  (:use :common-lisp)
  (:export #:scan)
  (:import-from :support :hex-char)))

(defpackage #:sparql-parser
  (:use :common-lisp))
