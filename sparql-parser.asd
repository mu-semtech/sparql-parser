(asdf:defsystem :sparql-parser
  :name "sparql-parser"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.21.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :license "MIT"
  :description "Parser for the SPARQL1.1 specification."
  :serial t
  :depends-on (alexandria cl-ppcre bordeaux-threads woo dexador jsown)
  :components ((:file "packages")
               ;; supporting code
               (:file "support/support")
               (:file "support/tree-db")
               ;; parsing an EBNF tree
               (:file "sparql-ast/ebnf")
               (:file "sparql-ast/terminals")
               (:file "sparql-ast/parser")
               (:file "sparql-ast/generator") ; output manipulated AST
               (:file "sparql-ast/manipulation")
               ;; sparql http - part 1
               (:file "connection/globals")
               (:file "connection/client")
               ;; access control definitions
               (:file "acl/acl")
               (:file "acl/config")
               ;; reasoning to determine graphs
               (:file "reasoner/tree-mirror")
               (:file "reasoner/prefixes")
               (:file "reasoner/term-info-types")
               (:file "reasoner/term-info")
               (:file "reasoner/reasoner")
               ;; updates
               (:file "updates/framework")
               (:file "updates/detect-quads")
               (:file "updates/handle-update-unit")
               ;; messenger
               (:file "delta/messenger")
               ;; visualizing resulting reasonings
               (:file "external/vis-js")
               ;; sparql http - part 2
               (:file "connection/server")))
