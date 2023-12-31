(defpackage :sparql-parser-test-scenario-a
  (:use :common-lisp)
  (:export
   #:run-assertion-tests))

(in-package :sparql-parser-test-scenario-a)

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
                   }
                   GRAPH ?g { ?s ?p ?o. }
                 }"
                 #-be-cautious 'base-string #+be-cautious 'string)))

(defmacro with-acl-config (&body body)
  "Executes body with the access rights specification required for these tests."

  `(let ((acl::*prefixes* nil)
         (acl::*access-specifications* nil)
         (acl::*graphs* nil)
         (acl::*rights* nil))
     ;; initialize rights
     (acl::define-prefixes
       :foaf "http://xmlns.com/foaf/0.1/"
       :authors "http://example.com/authors/"
       :ext "http://mu.semte.ch/vocabularies/ext/"
       :schema "http://schema.org/"
       :books "http://example.com/books/"
       :favorites "http://mu.semte.ch/favorites/")

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
       ("schema:Book" acl::-> acl::_))
     (acl:define-graph acl::user-data ("http://mu.semte.ch/graphs/personal/")
       (acl::_
        acl::-> "ext:hasBook"
        acl::-> "ext:hasSuperFavorite")
       ("foaf:Person" acl::<- "ext:hasFavoriteAuthor"))

     (acl:grant (acl::read acl::write)
                :to acl::public-data
                :for "admin")
     (acl:grant (acl::read)
                :to acl::public-data
                :for "public")
     (acl:grant (acl::read acl::write)
                :to acl::user-data
                :for "user")

     ,@body))

(defmacro with-impersonation-for (user &body body)
  "Impersonates USER."
  `(server::with-call-context
       (:mu-session-id (getf *known-session-ids* ,user))
     ,@body))

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
                      accounts:januuid1 mu:uuid \"janeuuid1\".
                      sessions:adminuuid session:account accounts:adminuuid1.
                      accounts:adminuuid1 ext:hasRole ext:Administrator;
                        mu:uuid \"adminuuid1\".
                    }
                  }"
                 #-be-cautious 'base-string #+be-cautious 'string)))

;;;; Scenario
;;;; Boot up a container using:
;;;; docker run --name virtuoso -p 8891:8890 -e SPARQL_UPDATE=true -e "DEFAULT_GRAPH=http://mu.semte.ch/application" redpencil/virtuoso:1.0.0
(defun run-assertion-tests ()
  (clean-up-graphs)
  (store-initial-session-data)

  (with-acl-config
    (format t "~&Joll is an administrator.~%")
    (with-impersonation-for :joll
      (format t "~&Can add authors.~%")

      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>

        INSERT DATA {
          authors:david a foaf:Person;
            foaf:name \"David Allen\".
          authors:steven a foaf:Person;
            foaf:name \"Steven Kotler\".
          authors:daniel a foaf:Person;
            foaf:name \"Daniel Kahneman\".
        }")

      (format t "~&Can add authors. (2)~%")
      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>

        INSERT DATA {
          books:gtd a schema:Book;
            schema:name \"Getting Things Done\";
            schema:creator authors:david.
          books:abundance a schema:Book;
            schema:name \"Abundance\";
            schema:creator authors:steven.
          books:fastAndSlow a schema:Book;
            schema:name \"Thinking Fast and Slow\";
            schema:creator authors:daniel.
        }")

      (format t "~&Can add extra book for author.~%")

      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>

        INSERT DATA {
          books:ready a schema:Book;
            schema:name \"Ready for Anything\";
            schema:creator authors:david .
        }")

      (format t "~&Can add extra author to book.~%")

      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>

        INSERT DATA {
          authors:peter a foaf:Person;
            schema:name \"Peter Diamantis\".
          books:abundance schema:creator authors:steven, authors:peter.
        }"))

    (with-impersonation-for :jack
      (format t "~&Jack is a user.~%")

      (format t "~&Jack can add a favorite.~%")
      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        INSERT DATA {
          favorites:me ext:hasBook books:gtd, books:fastAndSlow.
        }")

      ;; jack likes all authors of the book Abundance
      (format t "~&Jack can add conditional favorite authors.~%")
      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        INSERT {
          favorites:me ext:hasFavoriteAuthor ?author.
        } WHERE {
          books:abundance schema:creator ?author.
        }")
      ;; this data has no place to live, the target must be a foaf:Person and it is a book.
      (format t "~&Jack can't add books as favorite author.~%")
      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        INSERT {
          favorites:me ext:hasFavoriteAuthor ?book.
        } WHERE {
          books:abundance schema:creator/^schema:creator ?book.
        }")
      ;; let's check if jack has favorite authors
      (format t "~&Jack can ask for favorite authors.~%")
      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        ASK {
          favorites:me ext:hasFavoriteAuthor ?book. 
        }")      
      ;; then let's describe the values
      (format t "~&Jack can describe favorite authors.~%")
      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        DESCRIBE ?book {
          favorites:me ext:hasFavoriteAuthor ?book. 
        }")
      
      )))

