(defpackage :sparql-parser-test-utils
  (:use :common-lisp :fiveam)
  (:export
   #:*known-session-ids*
   #:clean-up-graphs
   #:with-acl-config
   #:with-impersonation-for
   #:db-test
   #:store-initial-session-data
   #:*run-geosparql-tests*
   ))

(in-package :sparql-parser-test-utils)

;;;;TODO: update this documentation
;;;; Represents a test scenario for the SPARQL parser
;;;;
;;;; This scenario writes books into the store.  Each book may have one
;;;; or many authors.
;;;;
;;;; Each user has their own graph in the database for their private
;;;; information, this includes stars given to each book.
;;;;
;;;; 1. We will let the administrator edit the public graph and add books.
;;;; 2. We will let the administrator update books.
;;;; 3. We will let a user add a star
;;;; 4. We will let a user add a star to each book written by the various authors
;;;;
;;;; Our users are :jack (user), :jane (user), and :joll (admin)
(defparameter *known-session-ids*
  (list
   :jack "http://mu.semte.ch/sessions/jackuuid"
   :jane "http://mu.semte.ch/sessions/janeuuid"
   :joll "http://mu.semte.ch/sessions/adminuuid"))

(defun clean-up-graphs ()
  (client:query (coerce
                 "DELETE {
                   GRAPH ?g { ?s ?p ?o }
                 } WHERE {
                   VALUES ?g {
                     <http://mu.semte.ch/graphs/public>
                     <http://mu.semte.ch/graphs/personal/jackuuid1>
                     <http://mu.semte.ch/graphs/personal/janeuuid1>
                     <http://mu.semte.ch/graphs/personal/adminuuid1>
                     <http://mu.semte.ch/graphs/account-info>
                     <http://mu.semte.ch/graphs/push>
                   }
                   GRAPH ?g { ?s ?p ?o. }
                 }"
                 #-be-cautious 'base-string #+be-cautious 'string)))

(defmacro with-acl-config (&body body)
  "Executes body with the access rights specification required for these tests."
  `(let ((prefix::*prefixes* nil)
         (acl::*access-specifications* nil)
         (acl::*graphs* nil)
         (acl::*rights* nil)
         (delta-messenger::*delta-handlers* nil)
         (client::*backend* (or (uiop:getenv "SPARQL_TEST_BACKEND") "http://localhost:8891/sparql"))
         (client::*log-sparql-query-roundtrip* nil)
         (type-cache::*uri-graph-user-type-providers* nil)
         (quad-transformations::*user-quad-transform-functions* nil))

     (type-cache::add-type-for-prefix "http://book-store.example.com/books/" "http://schema.org/Book")

     (quad-transformations:define-quad-transformation (quad method)
       ;; make quad objects which have datatype in uuid specification just strings
       (if (and
            ;; predicate is uuid
            (string= (quad-term:uri (quad:predicate quad))
                     "http://mu.semte.ch/vocabularies/core/uuid")
            ;; object has datatype
            (= (length (sparql-parser:match-submatches (quad:object quad))) 3))
           (let ((new-quad (quad:copy quad))) ; make new quad
             (setf (quad:object new-quad)
                   (sparql-manipulation:make-nested-match
                    `(ebnf::|RDFLiteral| ,(first (sparql-parser:match-submatches (quad:object quad))))))
             ;; use the new quad
             (quad-transformations:update new-quad))
           ;; otherwise keep it
           (quad-transformations:keep)))

     ;; initialize rights
     (acl::define-prefixes
       :foaf "http://xmlns.com/foaf/0.1/"
       :authors "http://example.com/authors/"
       :ext "http://mu.semte.ch/vocabularies/ext/"
       :schema "http://schema.org/"
       :books "http://example.com/books/"
       :favorites "http://mu.semte.ch/favorites/"
       :push "http://mu.semte.ch/vocabularies/push/"
       :geo "http://www.opengis.net/ont/geosparql#")

     (acl:supply-allowed-group "public")

     (acl:supply-allowed-group "user"
       :parameters ("id")
       :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
               PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
               SELECT ?id WHERE {
                 <SESSION_ID> session:account/mu:uuid ?id.
               }")

     (acl:supply-allowed-group "admin"
       :parameters ()
       :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
               PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
               PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
               SELECT ?account WHERE {
                 <SESSION_ID> session:account ?account.
                 ?account ext:hasRole ext:Administrator.
               }")

     (acl:define-graph acl::public-data ("http://mu.semte.ch/graphs/public")
       ("foaf:Person" acl::-> acl::_)
       ("schema:Book" acl::-> acl::_)
       ("geo:Geometry" acl::-> acl::_))

     (acl:define-graph acl::user-data ("http://mu.semte.ch/graphs/personal/")
       (acl::_
        acl::-> "ext:hasBook"
        acl::-> "ext:hasSuperFavorite"
        acl::-> "ext:longContent")
       ("foaf:Person" acl::<- "ext:hasFavoriteAuthor")
       ("ext:NoNameOrLabel" acl::x> "ext:name" acl::x> "ext:label"))

     (acl:define-graph acl::push-updates ("http://mu.semte.ch/graphs/push" :delta t :sparql nil)
       ("push:Update" acl::-> acl::_))

     (acl:grant (acl::read acl::write)
                :to acl::public-data
                :for "admin")
     (acl:grant (acl::read)
                :to acl::public-data
                :for "public")
     (acl:grant (acl::read acl::write)
                :to acl::user-data
                :for "user")

     ;; NOTE: in practice this would likely be scoped
     (acl:grant (acl::read acl::write)
                :to acl::push-updates
                :for "public")

     ,@body))

(defmacro with-impersonation-for (user &body body)
  "Impersonates USER."
  `(server::with-call-context
       (:mu-session-id (getf *known-session-ids* ,user))
     ,@body))

(defmacro db-test (testname (&rest options) &body body)
  "Runs BODY inside a fiveam test inside a freshly made acl config
  with cleaned up graphs and freshly stored initial session data."
  `(def-test ,testname (,@options)
     (with-acl-config
       (clean-up-graphs)
       (store-initial-session-data)
       ,@body)))

(defun store-initial-session-data ()
  "Stores the initial session data in the triplestore."
  (client:query (coerce
                 "PREFIX sessions: <http://mu.semte.ch/sessions/>
                  PREFIX session: <http://mu.semte.ch/vocabularies/session/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX accounts: <http://mu.semte.ch/vocabularies/ext/accounts/>

                  INSERT DATA {
                    GRAPH <http://mu.semte.ch/graphs/account-info> {
                      sessions:jackuuid session:account accounts:jackuuid1.
                      accounts:jackuuid1 mu:uuid \"jackuuid1\".
                      sessions:janeuuid session:account accounts:janeuuid1.
                      accounts:janeuuid1 mu:uuid \"janeuuid1\".
                      sessions:adminuuid session:account accounts:adminuuid1.
                      accounts:adminuuid1 ext:hasRole ext:Administrator;
                        mu:uuid \"adminuuid1\".
                    }
                  }"
                 #-be-cautious 'base-string #+be-cautious 'string)))

(defparameter *run-geosparql-tests* nil
  "These require geosparql support.  Should work with nbittich/virtuoso at
this point and likely a redpencil image too.")
