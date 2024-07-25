(in-package :cl-user)

(push :be-cautious *features*)

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
           #:pick-lists
           #:report-exponential-backoff-failure
           #:with-exponential-backoff-retry
           #:jsown-dedup
           #:depth-first-search
           #:case+
           #:group-by-size-and-count
           #:*string-max-size*
           #:*file-abbreviation-uri-prefix*
           #:maybe-uri-to-string
           #:maybe-string-to-uri))

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
           #:terminal-match-string
           #:match-match-submatches
           #:make-sparql-ast
           #:*scanning-string*
           #:match-term-p
           #:make-match
           #:make-scanned-token
           #:scanned-token-p
           #:copy-match))

(defpackage #:sparql-generator
  (:use :common-lisp)
  (:export #:is-valid #:write-valid
           #:write-when-valid
           #:write-valid-match))

(defpackage #:sparql-inspection
  (:use :common-lisp)
  (:import-from #:sparql-parser
                #:match
                #:scanned-token
                #:match-submatches
                #:match-term
                #:scanned-token-token
                #:scanned-token-effective-string)
  (:export #:match-equal-p
           #:ebnf-string-real-string
           #:first-found-scanned-token
           #:rdf-literal-datatype
           #:ebnf-numeric-literal-extract-info
           #:ebnf-simple-string-p))

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
           #:with-named-child
           #:expanded-term-case
           #:iriref
           #:make-iri
           #:make-word-match
           #:uri-wrap-marks
           #:uri-unwrap-marks
           #:make-var
           #:make-match-up-to-scanned-token
           #:make-token-match
           #:make-string-literal
           #:make-rdfliteral))

(defpackage #:type-cache
  (:use :common-lisp)
  (:import-from #:alexandria
                #:when-let*
                #:when-let)
  (:local-nicknames (#:lhash #:org.shirakumo.luckless.hashtable))
  (:export
   #:types-for
   #:update-known-types))

;; Server and client
(defpackage #:connection-globals
  (:use :common-lisp)
  (:export #:mu-call-id #:mu-session-id #:mu-auth-sudo #:mu-auth-allowed-groups #:mu-call-scope #:with-call-context #:mu-call-id-trail))

(defpackage #:acl-config
  (:use :common-lisp))

(defpackage #:prefix
  (:use :common-lisp)
  (:export #:expand
           #:*uri-protocol-accept-list-for-prefix-expansion*
           #:*uri-protocol-check-on-prefix-expansion*
           #:define-prefixes
           #:*prefixes*
           #:define-prefix))

(defpackage #:acl
  (:use :common-lisp #:sparql-manipulation #:connection-globals)
  (:import-from #:support #:-> #:jsown-dedup)
  (:import-from #:prefix #:define-prefixes)
  (:export #:apply-access-rights
           #:with-test-code-json-access-tokens
           #:dispatch-quads
           #:_
           #:supply-allowed-groups*
           #:define-prefixes
           #:define-graph
           #:with-scope
           #:grant*
           #:grant
           #:supply-allowed-group
           #:*uri-protocol-check-on-prefix-expansion*
           #:*uri-protocol-accept-list-for-prefix-expansion*))

(defpackage #:reasoner-tree-mirror
  (:use :common-lisp)
  (:export #:construct-reasoner-ast
           #:reasoner-ast-node
           #:reasoner-ast-parent
           #:reasoner-ast-children
           #:reasoner-ast-term-info-list
           #:reasoner-ast-dirty-p
           #:reasoner-ast
           #:tree-scan-deep-term-case
           #:with-named-child-tree
           #:do-grouped-tree-children
           #:loop-tree-matches-symbol-case
           #:tree-match-symbol-case)
  (:import-from #:sparql-parser
                #:match-term
                #:match-p
                #:sparql-ast
                #:sparql-ast-top-node
                #:match
                #:match-match-submatches)
  (:import-from #:alexandria
                #:compose
                #:when-let
                #:rcurry))

(defpackage #:reasoner-prefixes
  (:use :common-lisp)
  (:import-from #:sparql-manipulation
                #:make-iri
                #:expanded-term-case
                #:with-named-child
                #:do-grouped-children
                #:loop-matches-symbol-case)
  (:import-from #:support
                #:->)
  (:import-from #:reasoner-tree-mirror
                #:reasoner-ast-node
                #:loop-tree-matches-symbol-case
                #:reasoner-ast-term-info-list)
  (:export :cached-expanded-uri
           :extract-prefixes
           :with-local-prefixes
           :derive-expanded-uris
           :with-known-local-prefixes))

(defpackage #:reasoner-term-info
  (:use :common-lisp)
  ;; accounting
  (:export #:with-match-term-info
           #:print-term-info)
  ;; inspecting
  (:export #:term-info)
  ;; operations
  (:export #:join-or-term-info-statements
           #:union-term-info
           #:add-subject-predicate-object)
  ;; change tracking
  (:export #:with-term-info-change-tracking
           #:term-info-tracking-contains
           #:term-info-tracking-enabled
           #:term-info-tracking-empty-p
           #:term-info-tracking-get-current-tracker)
  ;; imports
  (:import-from #:sparql-parser
                #:scanned-token
                #:match
                #:match-match-submatches)
  (:import-from #:alexandria
                #:set-equal
                #:hash-table-keys)
  (:import-from #:support
                #:pick-lists
                #:typed-list
                #:typed-plist
                #:typed-hash-table
                #:with-derived-types
                #:->
                #:group-by)
  (:import-from #:reasoner-prefixes
                #:cached-expanded-uri)
  (:import-from #:reasoner-tree-mirror
                #:reasoner-ast-dirty-p
                #:reasoner-ast
                #:reasoner-ast-term-info-list))

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
                #:match-submatches
                #:match-match-submatches)
  (:import-from #:reasoner-prefixes
                #:cached-expanded-uri
                #:with-known-local-prefixes
                #:derive-expanded-uris
                #:extract-prefixes)
  (:import-from #:reasoner-tree-mirror
                #:do-grouped-tree-children
                #:with-named-child-tree
                #:tree-scan-deep-term-case
                #:reasoner-ast-node
                #:reasoner-ast-children
                #:construct-reasoner-ast))

(defpackage #:client
  (:use :common-lisp #:connection-globals)
  (:export #:query #:bindings
           #:batch-map-solutions-for-select-query
           #:batch-create-full-solution-for-select-query
           #:expand-bindings))

(defpackage #:server
  (:use :common-lisp #:connection-globals #:sparql-parser #:support #:client)
  (:import-from #:alexandria
                #:when-let)
  (:export #:execute-query-for-context #:boot))

(defpackage #:detect-quads
  (:use :common-lisp)
  (:import-from #:sparql-parser
                #:match-term
                #:terminal-match-string)
  (:export
   #:operation-type
   #:operation-data
   #:operation-data-subfield
   #:quad-term-uri=
   #:quad-term-uri))

(defpackage #:handle-update-unit
  (:use :common-lisp)
  (:import-from #:detect-quads
                #:operation-data-subfield
                #:quad-term-uri
                #:operation-type
                #:operation-data)
  (:import-from #:sparql-parser 
                #:make-match
                #:terminal-match-string))

(defpackage #:sparql-visjs
  (:use :common-lisp)
  (:import-from #:sparql-parser
                #:make-match
                #:scanned-token-effective-string
                #:scanned-token-token
                #:match-p
                #:match-submatches
                #:match-term
                #:scanned-token
                #:match)
  (:export #:match-as-visjs))

(defpackage :delta-messenger
  (:use :common-lisp)
  (:export :send-quads
           :delta-notify
           :add-delta-messenger
           :add-delta-logger))
