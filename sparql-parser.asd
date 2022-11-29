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
               (:file "support/support")
               (:file "support/tree-db")
               (:file "sparql-ast/ebnf")
               (:file "sparql-ast/terminals")
               (:file "sparql-ast/parser")
               (:file "sparql-ast/generator")
               (:file "sparql-ast/manipulation")
               (:file "connection/globals")
               (:file "reasoner/reasoner")
               (:file "acl/acl")
               (:file "acl/config")
               (:file "connection/server")
               (:file "connection/client")
               (:file "external/vis-js")))
