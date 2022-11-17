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
           #:read-bnfsexp-from-file
           #:rule-name #:read-bnfsexp-from-file #:rule-type #:rule-terminal-p #:rule-values-for #:rule-first #:rule-follow #:rule-expansion #:rule-index))

(defpackage :sparql-terminals
  (:use :common-lisp)
  (:export #:scan #:scanner-for #:scan-whitespace)
  (:import-from :support :hex-char))

(defpackage #:sparql-parser
  (:use :common-lisp)
  (:export #:sparql-ast #:sparql-ast-top-node #:sparql-ast-string #:clone-sparql-ast #:with-sparql-ast #:match #:match-p #:match-term #:print-match #:rule #:rule-p #:match-rule #:match-submatches #:scanned-token #:scanned-token-start #:scanned-token-end #:scanned-token-string #:scanned-token-token #:scanned-token-effective-string #:terminalp)
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
  (:export #:mu-call-id #:mu-session-id #:mu-auth-allowed-groups #:mu-call-scope #:with-call-context))

(defpackage #:acl-config
  (:use :common-lisp))

(defpackage #:acl
  (:use :common-lisp #:sparql-manipulation #:connection-globals)
  (:import-from #:support #:->)
  (:export #:apply-access-rights))

(defpackage #:client
  (:use :common-lisp #:connection-globals)
  (:export #:query))

(defpackage #:server
  (:use :common-lisp #:connection-globals #:sparql-parser #:support #:client))

