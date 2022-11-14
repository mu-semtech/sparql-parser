(in-package :cl-user)

(defpackage :tree-db
  (:use :common-lisp)
  (:shadow :equal)
  (:export #:create #:val #:keys-at #:all-keys #:equal #:copy))

(defpackage :support
  (:use :common-lisp)
  (:export #:-> #:debug-break #:embed-unicode-characters #:hex-char #:match-tree-search))

(defpackage :ebnf
  (:use :common-lisp)
  (:export #:rule #:terminal #:first #:follow #:seq #:alt #:|_eof| #:|_eps| #:|_empty| #:opt #:plus #:star
           #:read-bnfsexp-from-file))

(defpackage :sparql-terminals
  (:use :common-lisp)
  (:export #:scan #:scan-whitespace)
  (:import-from :support :hex-char))

(defpackage #:sparql-parser
  (:use :common-lisp)
  (:export #:match #:match-p #:match-term #:print-match #:rule #:rule-p #:match-rule #:match-submatches #:scanned-token #:scanned-token-start #:scanned-token-end #:scanned-token-string #:scanned-token-token #:terminalp)
  (:export #:parse-sparql-string #:with-parser-setup))

(defpackage #:sparql-generator
  (:use :common-lisp)
  (:export #:is-valid #:write-valid))

(defpackage #:sparql-manipulation
  (:use :common-lisp)
  (:export #:remove-dataset-clauses #:remove-graph-graph-patterns #:add-from-graphs))

;; Server and client
(defpackage #:connection-globals
  (:use :common-lisp)
  (:export #:mu-call-id #:mu-session-id #:mu-auth-allowed-groups #:with-call-context))

(defpackage #:client
  (:use :common-lisp #:connection-globals)
  (:export #:query))

(defpackage #:server
  (:use :common-lisp #:connection-globals #:sparql-parser #:support #:client #:sparql-manipulation))
