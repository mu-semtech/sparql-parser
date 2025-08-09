(asdf:defsystem :sparql-parser
  :name "sparql-parser"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.14"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :license "MIT"
  :description "Parser for the SPARQL1.1 specification."
  :serial t
  :depends-on (alexandria cl-ppcre bordeaux-threads woo dexador jsown luckless sha1 trivial-backtrace flexi-streams)
  :components ((:file "packages")
               ;; supporting code
               (:file "support/support")
               (:file "support/tree-db")
               (:file "support/string-file")
               (:file "support/vectors")
               (:file "support/semaphores")
               (:file "support/parallel-event-sequencer")
               (:file "support/woo-workers")
               ;; parsing an EBNF tree
               (:file "sparql-ast/ebnf")
               (:file "sparql-ast/terminals")
               (:file "sparql-ast/parser")
               (:file "sparql-ast/generator") ; output manipulated AST
               (:file "sparql-ast/inspection") ; TODO: refactor so inspection contains everything manipulation needs and move it earlier
               (:file "sparql-ast/manipulation")
               ;; sparql http - part 1
               (:file "connection/globals")
               (:file "connection/client")
               ;; caching
               (:file "cache/types")
               ;; support quads
               (:file "updates/quad-term")
               (:file "updates/quad")
               (:file "updates/quad-transformations")
               ;; access control definitions
               (:file "acl/prefix")
               (:file "acl/acl")
               (:file "acl/configuration-interface")
               (:file "acl/config")
               ;; ;; reasoning to determine graphs
               ;; (:file "reasoner/tree-mirror")
               ;; (:file "reasoner/prefixes")
               ;; (:file "reasoner/term-info-types")
               ;; (:file "reasoner/term-info")
               ;; (:file "reasoner/reasoner")
               ;; updates
               (:file "updates/framework")
               (:file "updates/detect-quads")
               (:file "updates/quad-operations-to-quads")
               (:file "updates/binding-match-transformation")
               (:file "updates/handle-update-unit")
               ;; messenger
               (:file "delta/message-bus")
               (:file "delta/messenger")
               ;; ;; visualizing resulting reasonings
               ;; (:file "external/vis-js")
               ;; sparql http - part 2
               (:file "connection/server")
               ;; shame
               (:file "support/monkeypatch")
               ;; administration
               (:file "administration/string-files")
               ;; configuration
               (:file "config/config")))
