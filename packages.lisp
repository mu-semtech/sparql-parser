(in-package :cl-user)

(defpackage :tree-db
  (:use :common-lisp)
  (:shadow :equal)
  (:export #:create #:val #:keys-at #:all-keys #:equal #:copy))

(defpackage :support
  (:use :common-lisp)
  (:export #:-> #:debug-break #:embed-unicode-characters #:hex-char #:match-tree-search #:group-by
           #:with-derived-types
           #:typed-hash-table
           #:typed-list
           #:typed-plist
           #:pick-lists))

(defpackage :ebnf
  (:use :common-lisp)
  (:export #:rule #:terminal #:first #:follow #:seq #:alt #:|_eof| #:|_eps| #:|_empty| #:opt #:plus #:star
           #:read-bnfsexp-from-file
           #:rule-name #:read-bnfsexp-from-file #:rule-type #:rule-terminal-p #:rule-values-for #:rule-first #:rule-follow #:rule-expansion #:rule-index
           #:define-abstract-token
           #:abstract-token-expansion))

(defpackage :sparql-terminals
  (:use :common-lisp)
  (:export #:scan #:scanner-for #:scan-whitespace)
  (:import-from :support :hex-char))

(defpackage #:sparql-parser
  (:use :common-lisp)
  (:export #:sparql-ast #:sparql-ast-top-node #:sparql-ast-string #:clone-sparql-ast #:with-sparql-ast #:match #:match-p #:match-term #:print-match #:rule #:rule-p #:match-rule #:match-submatches #:scanned-token #:scanned-token-start #:scanned-token-end #:scanned-token-string #:scanned-token-token #:scanned-token-effective-string #:terminalp)
  (:export #:parse-sparql-string #:with-parser-setup
           #:terminal-match-string))

(defpackage #:sparql-generator
  (:use :common-lisp)
  (:export #:is-valid #:write-valid
           #:write-when-valid
           #:write-valid-match))

(defpackage #:sparql-manipulation
  (:use :common-lisp)
  (:import-from #:sparql-parser
                #:match-submatches
                #:match-term
                #:match-p)
  (:export #:remove-dataset-clauses #:remove-graph-graph-patterns #:add-from-graphs #:replace-iriref
           #:add-default-base-decl-to-prologue
           #:loop-matches
           #:loop-matches-symbol-case
           #:do-grouped-children
           #:match-symbol-case
           #:with-named-child))

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

(defpackage #:reasoner-prefixes
  (:use :common-lisp)
  (:import-from #:sparql-manipulation
                #:with-named-child
                #:do-grouped-children
                #:loop-matches-symbol-case)
  (:import-from #:support
                #:->)
  (:export :cached-expanded-uri
           :extract-prefixes
           :with-local-prefixes
           :derive-expanded-uris
           :with-known-local-prefixes))

(defpackage #:reasoner-term-info
  (:use :common-lisp)
  (:export #:with-match-term-info
           #:term-info
           #:union-term-info
           #:add-subject-predicate-object
           #:print-term-info
           #:join-or-term-info-statements
           #:with-term-info-change-tracking
           #:term-info-tracking-contains
           #:term-info-tracking-enabled
           #:term-info-tracking-empty-p
           #:term-info-tracking-get-current-tracker)
  (:import-from #:sparql-parser
                #:scanned-token
                #:match)
  (:import-from #:alexandria
                #:set-equal
                #:hash-table-keys)
  (:import-from #:support
                #:typed-list
                #:typed-plist
                #:typed-hash-table
                #:with-derived-types
                #:->
                #:group-by)
  (:import-from #:reasoner-prefixes
                #:cached-expanded-uri))

(defpackage #:reasoner
  (:use :common-lisp)
  (:import-from #:support
                #:group-by
                #:->)
  (:import-from #:sparql-manipulation
                #:with-named-child
                #:match-symbol-case
                #:do-grouped-children
                #:loop-matches-symbol-case)
  (:import-from #:alexandria
                #:rcurry
                #:hash-table-keys
                #:when-let)
  (:import-from #:reasoner-term-info
                #:term-info-tracking-get-current-tracker
                #:with-term-info-change-tracking
                #:term-info-tracking-contains
                #:term-info-tracking-empty-p
                #:term-info
                #:union-term-info
                #:join-or-term-info-statements)
  (:import-from #:sparql-parser
                #:match
                #:match-p
                #:match-term
                #:match-submatches)
  (:import-from #:reasoner-prefixes
                #:cached-expanded-uri
                #:with-known-local-prefixes
                #:derive-expanded-uris
                #:extract-prefixes))

(defpackage #:client
  (:use :common-lisp #:connection-globals)
  (:export #:query #:bindings))

(defpackage #:server
  (:use :common-lisp #:connection-globals #:sparql-parser #:support #:client)
  (:import-from #:alexandria
                #:when-let))

(defpackage #:sparql-visjs
  (:use :common-lisp)
  (:import-from #:sparql-parser
                #:scanned-token-effective-string
                #:scanned-token-token
                #:match-p
                #:match-submatches
                #:match-term
                #:scanned-token
                #:match)
  (:export #:match-as-visjs))
