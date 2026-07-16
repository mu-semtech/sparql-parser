(defpackage :sparql-parser-test-scenario-a
  (:use :common-lisp :fiveam)
  (:export
   #:run-tests))

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
         (client::*backend* "http://localhost:8891/sparql")
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

     ;; (quad-transformations:add-quad-processor
     ;;  (lambda (quad &key method)
     ;;    (declare (ignorable method))
     ;;    (labels ((quad-transformations:update (quad-transformations::quads)
     ;;               (cond ((null quad-transformations::quads) (values nil t t))
     ;;                     ((listp (first quad-transformations::quads))
     ;;                      (values quad-transformations::quads t t))
     ;;                     (t (values (list quad-transformations::quads) t t))))
     ;;             (quad-transformations:keep ()
     ;;               (values nil nil t))
     ;;             (quad-transformations::execute-body ()
     ;;               (multiple-value-bind
     ;;                     (quad-transformations::result
     ;;                      quad-transformations::update-quad-p
     ;;                      quad-transformations::used-internal-function-p)
     ;;                   (progn
     ;;                     (if (and
     ;;                          (string=
     ;;                           (detect-quads:quad-term-uri (quad:predicate quad))
     ;;                           "http://mu.semte.ch/vocabularies/core/uuid")
     ;;                          (=
     ;;                           (length
     ;;                            (sparql-parser:match-submatches (quad:object quad)))
     ;;                           3))
     ;;                         (let ((new-quad (quad:copy quad)))
     ;;                           (setf (quad:object new-quad)
     ;;                                 (sparql-manipulation:make-nested-match
     ;;                                  `(ebnf::|RDFLiteral|
     ;;                                          ,(first
     ;;                                            (sparql-parser:match-submatches
     ;;                                             (quad:object quad))))))
     ;;                           (quad-transformations:update new-quad))
     ;;                         (quad-transformations:keep)))
     ;;                 (unless quad-transformations::used-internal-function-p
     ;;                   (format t
     ;;                           "~&[ERROR][QUAD-PROCESSOR] Quad processor user function did not call internal replacement function REPLACE or KEEP. Ignoring possible changes.~%"))
     ;;                 (values quad-transformations::result
     ;;                         quad-transformations::update-quad-p))))
     ;;      (quad-transformations::execute-body))))

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

;;;; Boot up a container using:
;;;; docker run --name virtuoso -p 8891:8890 -e SPARQL_UPDATE=true -e "DEFAULT_GRAPH=http://mu.semte.ch/application" redpencil/virtuoso:1.2.0-rc.1; dr rm virtuoso
(def-suite test-suite-scenario-a-1)
(in-suite test-suite-scenario-a-1)

(def-test joll-can-add-authors-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)
    (with-impersonation-for :joll
      (finishes
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
          }"))

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            PREFIX authors: <http://example.com/authors/>
            ASK {
              authors:david a foaf:Person;
                foaf:name \"David Allen\".
              authors:steven a foaf:Person;
                foaf:name \"Steven Kotler\".
              authors:daniel a foaf:Person;
                foaf:name \"Daniel Kahneman\".
            }"))
           "boolean")))))

(def-test joll-can-add-authors2-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)
    (with-impersonation-for :joll
      (finishes
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
          }"))
      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>
              ASK {
                books:gtd a schema:Book;
                  schema:name \"Getting Things Done\";
                  schema:creator authors:david.
                books:abundance a schema:Book;
                  schema:name \"Abundance\";
                  schema:creator authors:steven.
                books:fastAndSlow a schema:Book;
                  schema:name \"Thinking Fast and Slow\";
                  schema:creator authors:daniel.
              }"))
           "boolean")))))

(def-test joll-can-add-extra-book-for-author-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)
    (with-impersonation-for :joll
      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>

          INSERT DATA {
            books:ready a schema:Book;
              schema:name \"Ready for Anything\";
              schema:creator authors:david .
          }"))
      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>

              ASK {
                books:ready a schema:Book;
                  schema:name \"Ready for Anything\";
                  schema:creator authors:david .
              }"))
           "boolean")))))

(def-test joll-can-add-extra-author-for-book-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)
    (with-impersonation-for :joll
      (finishes
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

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>

              ASK {
                authors:peter a foaf:Person;
                  schema:name \"Peter Diamantis\".
                books:abundance schema:creator authors:steven, authors:peter.
              }"))
           "boolean")))))


(def-suite test-suite-scenario-a-2)
(in-suite test-suite-scenario-a-2)

(def-test jack-can-add-favorite-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)
    (with-impersonation-for :jack
      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          INSERT DATA {
            favorites:me ext:hasBook books:gtd, books:fastAndSlow.
          }"))

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX books: <http://example.com/books/>
              PREFIX favorites: <http://mu.semte.ch/favorites/>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              ASK {
                favorites:me ext:hasBook books:gtd.
                favorites:me ext:hasBook books:fastAndSlow.
              }"))
           "boolean")))))

(def-test jack-can-add-conditional-favorite-authors-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)

    (with-impersonation-for :joll
      (finishes
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
          }"))

      (finishes
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
      )

    (with-impersonation-for :jack

      (finishes
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
          }"))

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX authors: <http://example.com/authors/>
              PREFIX favorites: <http://mu.semte.ch/favorites/>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              ASK {
                favorites:me ext:hasFavoriteAuthor authors:steven.
                favorites:me ext:hasFavoriteAuthor authors:peter.
              }"))
           "boolean")))))

(def-test jack-cant-add-books-as-favorite-author-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)

    (with-impersonation-for :joll
      (finishes
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
          }")))

    (with-impersonation-for :jack
      (signals
          handle-update-unit:unwritten-data-error
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
          }")))))


(def-test jack-can-ask-for-favorite-authors-test ()

  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)

    (with-impersonation-for :joll
      (finishes
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
          }")))

    (with-impersonation-for :jack
      (finishes
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
          }"))

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>
              PREFIX favorites: <http://mu.semte.ch/favorites/>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

              ASK {
                favorites:me ext:hasFavoriteAuthor ?author.
              }"))
           "boolean")))))

(def-test jack-can-describe-favorite-authors-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)

    (with-impersonation-for :joll
      (finishes
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
          }")))

    (with-impersonation-for :jack
      (finishes
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
          }"))
      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          DESCRIBE ?author {
            favorites:me ext:hasFavoriteAuthor ?author.
          }")))))

(def-test jack-can-execute-delete-where-and-insert-data-in-one-query-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (finishes
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
          }")))

    (with-impersonation-for :jack
      (finishes
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
          }"))

      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          DELETE {
            favorites:me ext:hasFavoriteAuthor ?book.
          } WHERE {
            favorites:me ext:hasFavoriteAuthor ?book.
          };
          INSERT DATA {
            GRAPH <http://mu.semte.ch/application> {
              favorites:me ext:hasFavoriteAuthor authors:david.
            }
          }"))

      (is (not (jsown:val
                (jsown:parse
                 (server:execute-query-for-context
                  "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                    PREFIX schema: <http://schema.org/>
                    PREFIX authors: <http://example.com/authors/>
                    PREFIX books: <http://example.com/books/>
                    PREFIX favorites: <http://mu.semte.ch/favorites/>
                    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

                    ASK {
                      favorites:me ext:hasFavoriteAuthor authors:steven.
                    }"))
                "boolean")))

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>
              PREFIX favorites: <http://mu.semte.ch/favorites/>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

              ASK {
                favorites:me ext:hasFavoriteAuthor authors:david.
              }"))
           "boolean")))))


(def-suite test-suite-scenario-a-3)
(in-suite test-suite-scenario-a-3)

(def-test joll-can-write-a-book-title-with-the-right-uri-and-no-type-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll

      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          INSERT DATA {
            <http://book-store.example.com/books/my-book> schema:name \"On Types\".
          }"))

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>
              PREFIX favorites: <http://mu.semte.ch/favorites/>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

              ASK {
                <http://book-store.example.com/books/my-book> schema:name \"On Types\".
              }"))
           "boolean")))))

(def-test changes-contain-only-the-data-that-was-actually-changed-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          INSERT DATA {
            <http://book-store.example.com/books/my-book> schema:name \"On Types\", \"On Types Too\".
          }"))


      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>
              PREFIX favorites: <http://mu.semte.ch/favorites/>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

              ASK {
                <http://book-store.example.com/books/my-book> schema:name \"On Types\", \"On Types Too\".
              }"))
           "boolean")))))


(def-test reinserting-long-content-does-not-duplicate-abbreviation-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (let ((support:*string-max-size* 50))
        (finishes
          (server:execute-query-for-context
           "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
            INSERT DATA {
              <http://book-store.example.com/books/my-book> ext:longContent \"This is a string which has more than 50 characters in length\", \"String < 50 chars\" .
            }"))
        (finishes
          (server:execute-query-for-context
           "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
            INSERT DATA {
              <http://book-store.example.com/books/my-book> ext:longContent \"This is a string which has more than 50 characters in length\", \"String < 50 chars\" .
            }"))
        (is (= 1 (parse-integer
                  (jsown:val
                   (jsown:val
                    (first (jsown:filter
                            (jsown:parse
                             (server:execute-query-for-context
                              "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                               SELECT (COUNT(DISTINCT ?content) AS ?count) WHERE {
                                 <http://book-store.example.com/books/my-book> ext:longContent ?content.
                                 FILTER(isURI(?content))
                               }"))
                            "results" "bindings"))
                    "count")
                   "value"))))))))

(def-test joll-can-collapse-multiple-tiles-via-delete-insert-where-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          INSERT DATA {
            <http://book-store.example.com/books/my-book> schema:name \"On Types\".
          }"))

      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          DELETE {
            <http://book-store.example.com/books/my-book> schema:name ?title.
          } INSERT {
            <http://book-store.example.com/books/my-book> schema:name \"On Types\".
          } WHERE {
            <http://book-store.example.com/books/my-book> schema:name ?title.
          }"))
      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              PREFIX schema: <http://schema.org/>
              PREFIX authors: <http://example.com/authors/>
              PREFIX books: <http://example.com/books/>
              PREFIX favorites: <http://mu.semte.ch/favorites/>
              PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

              ASK {
                <http://book-store.example.com/books/my-book> schema:name \"On Types\".
                FILTER NOT EXISTS {
                  <http://book-store.example.com/books/my-book> schema:name ?other.
                  FILTER (?other != \"On Types\")
                }
              }"))
           "boolean")))))


(def-test we-can-delete-the-types-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          INSERT DATA {
            <http://book-store.example.com/books/my-book> schema:name \"On Types\".
          }"))

      (finishes
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          DELETE WHERE {
            <http://book-store.example.com/books/my-book> schema:name \"On Types\".
          }"))
      (is (not (jsown:val
                (jsown:parse
                 (server:execute-query-for-context
                  "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                   PREFIX schema: <http://schema.org/>
                   PREFIX authors: <http://example.com/authors/>
                   PREFIX books: <http://example.com/books/>
                   PREFIX favorites: <http://mu.semte.ch/favorites/>
                   PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

                   ASK {
                     <http://book-store.example.com/books/my-book> schema:name \"On Types\".
                   }"))
                "boolean"))))))


(def-test we-can-have-an-empty-construct-where-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (finishes
        (server:execute-query-for-context
         "CONSTRUCT { } WHERE { }")))))

;; ;; TODO: is this ok?
(def-test inserting-the-uuid-will-just-insert-the-uuid-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (finishes
        (server:execute-query-for-context
         "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
     PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
     INSERT DATA { <http://book-store.example.com/books/my-book> mu:uuid \"123\"^^xsd:string. }"))
      (let ((binding (first (jsown:filter
                             (jsown:parse
                              (server:execute-query-for-context
                               "PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                             SELECT ?uuid WHERE { <http://book-store.example.com/books/my-book> mu:uuid ?uuid }"))
                             "results" "bindings"))))
        (is (string= "literal" (jsown:val (jsown:val binding "uuid") "type")))
        (is (jsown:val (jsown:val binding "uuid") "value") "123")))))


(def-test geo-sparql-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :joll
      (if *run-geosparql-tests*
          (progn
            (finishes
              (server:execute-query-for-context
               "PREFIX geo: <http://www.opengis.net/ont/geosparql#>
                INSERT DATA {
                  <http://book-store.example.com/geometries/a>
                      a geo:Geometry;
                      geo:asWKT \"<https://www.opengis.net/def/crs/EPSG/0/31370> POINT (155822.2 132723.18)\"^^geo:wktLiteral.
                  }"))
            (is (jsown:val
                 (jsown:parse
                  (server:execute-query-for-context
                   "PREFIX geo: <http://www.opengis.net/ont/geosparql#>
                ASK {
                  <http://book-store.example.com/geometries/a> geo:asWKT
                    \"<http://www.opengis.net/def/crs/EPSG/0/31370> POINT (155822.2 132723.18)\"^^geo:wktLiteral.
                }"))
                 "boolean")))
          (is (identity t))))))

(def-suite test-suite-scenario-a-4)
(in-suite test-suite-scenario-a-4)

(def-test can-insert-some-random-content-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :jack
      (finishes
        (server:execute-query-for-context
         "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
           INSERT DATA {
             ext:myDisplay a ext:NoNameOrLabel;
               ext:score 9001;
               ext:level 12.
           }"))
      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                ASK {
                  ext:myDisplay a ext:NoNameOrLabel;
                    ext:score 9001;
                    ext:level 12.
                }"))
           "boolean")))))

(def-test jack-cant-add-name-to-nonameorlabel-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :jack
      (signals handle-update-unit:unwritten-data-error
        (server:execute-query-for-context
         "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          INSERT DATA {
            ext:myDisplay ext:name \"Failing name\".
          }")))))

(def-test jack-cant-add-label-to-nonameorlabel-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :jack
      (signals handle-update-unit:unwritten-data-error
        (server:execute-query-for-context
         "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          INSERT DATA {
            ext:myDisplay ext:label \"Failing label\".
          }")))))

(def-test jack-can-add-other-predicates-to-nonameorlabel-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :jack
      (finishes
        (server:execute-query-for-context
         "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          INSERT DATA {
            ext:myDisplay ext:anotherThing \"Another thing\".
          }"))

      (is (jsown:val
           (jsown:parse
            (server:execute-query-for-context
             "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              ASK {
                ext:myDisplay ext:anotherThing \"Another thing\".
              }"))
           "boolean")))))


(def-test jack-can-delete-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :jack
      (finishes
        (server:execute-query-for-context
         "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          DELETE {
            ext:myDisplay ext:score ?score; ext:level ?level.
          } WHERE {
            ext:myDisplay a ext:NoNameOrLabel;
              ext:score ?score;
              ext:level ?level.
          }"))

      (is (not (jsown:val
                (jsown:parse
                 (server:execute-query-for-context
                  "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                   ASK {
                   ext:myDisplay a ext:NoNameOrLabel;
                     ext:score ?score;
                     ext:level ?level.
                   }"))
                "boolean"))))))

(def-suite test-suite-scenario-a-5)
(in-suite test-suite-scenario-a-5)

(def-test coerce-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (finishes
      (client:query (coerce
                     "DELETE {
                        GRAPH ?g { ?s ?p ?o }
                      } WHERE {
                        VALUES ?g {
                          <http://mu.semte.ch/graphs/push>
                        }
                        GRAPH ?g { ?s ?p ?o. }
                      }" #-be-cautious 'base-string #+be-cautious 'string)))))

(def-suite test-suite-scenario-a-6)
(in-suite test-suite-scenario-a-6)

(def-test can-insert-a-push-update-test ()
  (with-acl-config
    (clean-up-graphs)
    (store-initial-session-data)


    (with-impersonation-for :jack
      (finishes
        (server:execute-query-for-context
         "PREFIX push: <http://mu.semte.ch/vocabularies/push/>
          PREFIX dct: <http://purl.org/dc/terms/>
          INSERT DATA {
            push:myUpdate a push:Update;
              dct:title \"Receive delta without writing\".
          }"))

      (is (not (jsown:val
                (jsown:parse
                 (client:query
                  (coerce
                   "PREFIX push: <http://mu.semte.ch/vocabularies/push/>
                    ASK { GRAPH <http://mu.semte.ch/graphs/push> {
                      push:myUpdate a push:Update.
                    } }"
                   #-be-cautious 'base-string #+be-cautious 'string)))
                "boolean")))

      (is (= 0
             (length
              (jsown:filter
               (jsown:parse
                (server:execute-query-for-context
                 "PREFIX push: <http://mu.semte.ch/vocabularies/push/>
              SELECT * WHERE {
                ?thing a push:Update.
              }"))
               "results" "bindings")))))))


(defun run-tests ()
  (let (results)
    (quad-transformations:define-quad-transformation (quad method)
      ;; fix wktLiteral string representation
      (let* ((object (quad:object quad))
             (datatype-match (and
                              (sparql-parser:match-p object)
                              (eq (sparql-parser:match-term object) 'ebnf::|RDFLiteral|)
                              (= 3 (length (sparql-parser:match-submatches object)))
                              (third (sparql-parser:match-submatches object))))
             (datatype-uri (and datatype-match
                                (quad-term:uri
                                 (first
                                  (sparql-parser:match-submatches datatype-match)))))
             (string-value (and (sparql-parser:match-p object)
                                (eq (sparql-parser:match-term object) 'ebnf::|RDFLiteral|)
                                (sparql-manipulation:string-literal-string
                                 (first (sparql-parser:match-submatches object))))))
        (if (and datatype-uri
                 (string= "http://www.opengis.net/ont/geosparql#wktLiteral" datatype-uri)
                 (search "https://www.opengis.net/" string-value))
            (let ((new-quad (quad:copy quad))
                  (new-string (cl-ppcre:regex-replace "https://" string-value "http://")))
              (setf (quad:object new-quad)
                    (sparql-manipulation:make-rdfliteral new-string :datatype-match datatype-match))
              (quad-transformations:update new-quad))
            (quad-transformations:keep))))

    (push (run! 'test-suite-scenario-a-1) results)

    (push (run! 'test-suite-scenario-a-2) results)

    (push (run! 'test-suite-scenario-a-3) results)

    (push (run! 'test-suite-scenario-a-4) results)

    (push (run! 'test-suite-scenario-a-5) results)

    (push (run! 'test-suite-scenario-a-6) results)

    (every #'identity results)))
