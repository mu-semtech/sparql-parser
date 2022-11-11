(in-package :cl-user)

(defpackage :tree-db
  (:use :common-lisp)
  (:shadow :equal)
  (:export #:create #:val #:keys-at #:all-keys #:equal #:copy))

(defpackage :support
  (:use :common-lisp)
  (:export #:-> #:debug-break #:embed-unicode-characters #:hex-char #:match-tree-search #:read-bnfsexp-from-file))

(defpackage :sparql-bnf
  (:export #:|rule| #:|terminal| #:|first| #:|follow| #:|seq| #:|alt| #:|_eof| #:|_eps| #:|_empty| #:|opt| #:|plus| #:|star|))

(defpackage :sparql-terminals
  (:use :common-lisp)
  (:export #:scan #:scan-whitespace)
  (:import-from :support :hex-char))

(defpackage #:sparql-parser
  (:use :common-lisp)
  (:export #:match #:match-p #:match-term #:print-match #:rule #:rule-p #:match-rule #:match-submatches #:scanned-token #:scanned-token-start #:scanned-token-end #:scanned-token-string #:scanned-token-token #:terminalp)
  (:export #:parse-sparql-string))

(defpackage #:sparql-generator
  (:use :common-lisp)
  (:export #:is-valid #:write-valid))

(defpackage #:sparql-manipulation
  (:use :common-lisp))

(defpackage #:server
  (:use :common-lisp))

(defpackage #:client
  (:use :common-lisp))
