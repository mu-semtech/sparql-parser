(asdf:defsystem :sparql-parser
  :name "sparql-parser"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.21.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :license "MIT"
  :description "Parser for the SPARQL1.1 specification."
  :serial t
  :depends-on (alexandria cl-ppcre)
  :components ((:file "packages")
               (:file "support")
               (:file "support/tree-db")
               (:file "sparql-terminals")
               (:file "parser")
               (:file "generator")))
