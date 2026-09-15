(defpackage :sparql-parser-test-graph-cleanup
  (:use
   :common-lisp
   :fiveam
   :sparql-parser-test-integration
   :sparql-parser-test-utils))

(in-package :sparql-parser-test-graph-cleanup)


(def-suite graph-cleanup :in integration-tests)
(in-suite graph-cleanup)

(db-test coerce-test ()
  (finishes
    (client:query
     (coerce
      "DELETE {
         GRAPH ?g { ?s ?p ?o }
       } WHERE {
         VALUES ?g {
           <http://mu.semte.ch/graphs/push>
         }
         GRAPH ?g { ?s ?p ?o. }
       }" #-be-cautious 'base-string #+be-cautious 'string))))
