(defpackage :sparql-parser-test-duplicates
  (:use
   :common-lisp
   :fiveam
   :sparql-parser-test-integration))

(in-package :sparql-parser-test-duplicates)

;;;; Represents a test scenario for the SPARQL parser
;;;;
;;;; This scenario writes data into multiple graphs.
;;;;
;;;; Any user is allowed to write anything to two graphs
(defun clean-up-graphs ()
  (client:query (coerce
                 "DELETE {
                   GRAPH ?g { ?s ?p ?o }
                 } WHERE {
                   VALUES ?g {
                     <http://mu.semte.ch/graphs/a>
                     <http://mu.semte.ch/graphs/b>
                   }
                   GRAPH ?g { ?s ?p ?o. }
                 }"
                 #-be-cautious 'base-string #+be-cautious 'string)))

(defmacro with-session-id (&body body)
  `(server::with-call-context
       (:mu-session-id "http://mu.semte.ch/sessions/42")
     ,@body))

(defmacro with-acl-config (&body body)
  "Executes body with the access rights specification required for these tests."
  `(let ((prefix::*prefixes* nil)
         (acl::*access-specifications* nil)
         (acl::*graphs* nil)
         (acl::*rights* nil)
         (delta-messenger::*delta-handlers* nil)
         (client::*backend* "http://localhost:8891/sparql")
         (client::*log-sparql-query-roundtrip* nil)
         (type-cache::*uri-graph-user-type-providers* nil)
         (quad-transformations::*user-quad-transform-functions* nil))

     ;; initialize rights
     (acl::define-prefixes
       :foaf "http://xmlns.com/foaf/0.1/"
       :ext "http://mu.semte.ch/vocabularies/ext/")

     (acl:supply-allowed-group "writea")
     (acl:supply-allowed-group "writeb")

     (acl:define-graph acl::graph-a ("http://mu.semte.ch/graphs/a")
       (acl::_ acl::-> acl::_))
     (acl:define-graph acl::graph-b ("http://mu.semte.ch/graphs/b")
       (acl::_ acl::-> acl::_))

     (acl:grant (acl::read acl::write)
                :to acl::graph-a
                :for "writea")
     (acl:grant (acl::read acl::write)
                :to acl::graph-b
                :for "writeb")
     ,@body))

(defparameter *run-geosparql-tests* nil
  "These require geosparql support.  Should work with nbittich/virtuoso at
this point and likely a redpencil image too.")

(def-suite duplicates :in integration-tests)
(in-suite duplicates)

(def-test ungraphed-insert-duplicates-into-all-writable-graphs-test ()
  (clean-up-graphs)
  (with-acl-config
    (with-session-id
      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          INSERT DATA {
            ext:me foaf:name \"\"\"Aad\"\"\".
          }"))
      (let* ((response (client:query (coerce
                                      "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                                       PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                                       SELECT ?g WHERE { GRAPH ?g { ext:me foaf:name \"Aad\". } }"
                                      #-be-cautious 'base-string #+be-cautious 'string)))
             (graphs (mapcar (lambda (binding) (jsown:val (jsown:val binding "g") "value"))
                             (jsown:filter (jsown:parse response) "results" "bindings"))))
        ;; all graphs have been edited
        (is (= 2 (length graphs)))
        ;; check if individual graphs are in the results
        (is (member "http://mu.semte.ch/graphs/a" graphs :test #'string=))
        (is (member "http://mu.semte.ch/graphs/b" graphs :test #'string=))))))
