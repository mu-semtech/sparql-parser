(defpackage :sparql-parser-test-scenario-b
  (:use :common-lisp)
  (:export
   #:run-assertion-tests))

(in-package :sparql-parser-test-scenario-b)

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
         (client::*log-sparql-query-roundtrip* t)
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

;;;; Scenario
;;;; Boot up a container using:
;;;; docker run --name virtuoso -p 8891:8890 -e SPARQL_UPDATE=true -e "DEFAULT_GRAPH=http://mu.semte.ch/application" redpencil/virtuoso:1.2.0-rc.1; dr rm virtuoso
(defun run-assertion-tests ()
  (clean-up-graphs)

  (with-acl-config
    (with-session-id
      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  
  
        INSERT DATA {
          ext:me foaf:name \"\"\"Aad\"\"\".
        }")
      (let ((response (server:execute-query-for-context
               "SELECT * WHERE { ?s ?p ?o. }")))
        (assert (= (length (jsown:filter (jsown:parse response)
                                         "results"
                                         "bindings"))
                   2))))))
