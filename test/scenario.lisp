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
  `(let ((prefix::*prefixes* nil)
         (acl::*access-specifications* nil)
         (acl::*graphs* nil)
         (acl::*rights* nil)
         (client::*backend* "http://localhost:8891/sparql")
         (client::*log-sparql-query-roundtrip* t)
         (type-cache::*uri-graph-user-type-providers* nil)
         (quad-transformations::*user-quad-transform-functions* nil))

     (type-cache::add-type-for-prefix "http://book-store.example.com/books/" "http://schema.org/Book")

     (quad-transformations:define-quad-transformation (quad method)
       ;; make quad objects which have datatype in uuid specification just strings
       (if (and
            ;; predicate is uuid
            (string= (detect-quads::quad-term-uri (quad:predicate quad))
                     "http://mu.semte.ch/vocabularies/core/uuid")
            ;; object has datatype
            (= (length (sparql-parser:match-submatches (quad:object quad))) 3))
           (let ((new-quad (quad:copy quad))) ; make new quad
             (setf (quad:object new-quad)
                   (handle-update-unit::make-nested-match
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
     ;;                                 (handle-update-unit::make-nested-match
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
;;;; docker run --name virtuoso -p 8891:8890 -e SPARQL_UPDATE=true -e "DEFAULT_GRAPH=http://mu.semte.ch/application" redpencil/virtuoso:1.2.0-rc.1; dr rm virtuoso
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
      (handler-case
          (progn
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
           (format t "~&ERROR: Oh noes, Jack shouldn't be allowed to do add a book as an author!~%"))
        (error (e) (declare (ignore e)) t))
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
          favorites:me ext:hasFavoriteAuthor ?author.
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

        DESCRIBE ?author {
          favorites:me ext:hasFavoriteAuthor ?author.
        }")

      ;; now let's replace the favorite author in two queries rather
      ;; than in one
      (format t "~&Jack can execute delete where and insert data in one query.~%")
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

    (with-impersonation-for :joll
      (quad-transformations:define-quad-transformation (quad method)
        ;; fix wktLiteral string representation
        (let* ((object (quad:object quad))
               (datatype-match (and
                                (sparql-parser:match-p object)
                                (eq (sparql-parser:match-term object) 'ebnf::|RDFLiteral|)
                                (= 3 (length (sparql-parser:match-submatches object)))
                                (third (sparql-parser:match-submatches object))))
               (datatype-uri (and datatype-match
                                  (detect-quads::quad-term-uri
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

      (format t "~&Joll can write a book title with the right URI and no type.~%")

      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        INSERT DATA {
         <http://book-store.example.com/books/my-book> schema:name \"On Types\".
       }")

      (format t "~&Effective changes contain only the data that was actually changed, which is:~%- insert \"On types too.\"~%- delete \"On types too.\"~%")

      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        INSERT DATA {
         <http://book-store.example.com/books/my-book> schema:name \"On Types\", \"On Types Too\".
       }")

      (let ((support:*string-max-size* 50))
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          INSERT DATA {
           <http://book-store.example.com/books/my-book> ext:longContent \"This is a string which has more than 50 characters in length\", \"String < 50 chars\" .
         }")

        (format t "~&Matches yield following content for long content: ~%~A"
                (server:execute-query-for-context
                 "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

                  SELECT ?content WHERE { <http://book-store.example.com/books/my-book> ext:longContent ?content }"))
        (server:execute-query-for-context
         "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX schema: <http://schema.org/>
          PREFIX authors: <http://example.com/authors/>
          PREFIX books: <http://example.com/books/>
          PREFIX favorites: <http://mu.semte.ch/favorites/>
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          INSERT DATA {
           <http://book-store.example.com/books/my-book> ext:longContent \"This is a string which has more than 50 characters in length\", \"String < 50 chars\" .
         }"))

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
        }")

      ;; we can delete the types

      (server:execute-query-for-context
       "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX schema: <http://schema.org/>
        PREFIX authors: <http://example.com/authors/>
        PREFIX books: <http://example.com/books/>
        PREFIX favorites: <http://mu.semte.ch/favorites/>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

        DELETE WHERE {
         <http://book-store.example.com/books/my-book> schema:name \"On Types\".
        }")

      ;; we can have an empty construct where

      (server:execute-query-for-context
       "CONSTRUCT { } WHERE { }")

      ;; inserting the UUID with xsd:string will just insert the UUID (configured above)

      (server:execute-query-for-context
       "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        INSERT DATA { <http://book-store.example.com/books/my-book> mu:uuid \"123\"^^xsd:string. }")

      (server:execute-query-for-context
       "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        PREFIX geo: <http://www.opengis.net/ont/geosparql#>
        INSERT DATA {
          <http://book-store.example.com/geometries/a>
             a geo:Geometry;
             geo:asWKT \"<https://www.opengis.net/def/crs/EPSG/0/31370> POINT (155822.2 132723.18)\"^^<http://www.opengis.net/ont/geosparql#wktLiteral>.
         }"))

    (with-impersonation-for :jack
      ;; can insert some random content
      (server:execute-query-for-context
       "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        INSERT DATA {
          ext:myDisplay a ext:NoNameOrLabel;
            ext:score 9001;
            ext:level 12.
        }")
      ;; can't insert name or label
      (block :no-error
        (handler-case
            (server:execute-query-for-context
             "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              INSERT DATA {
                ext:myDisplay ext:name \"Failing name\".
              }")
          (handle-update-unit:unwritten-data-error (e)
            (format t "Received expected error ~A" e)
            (return-from :no-error t)))
        (error 'simple-error :format-control "Expected triples not being written, but received no error."))
      (block :no-error
        (handler-case
            (server:execute-query-for-context
             "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
              INSERT DATA {
                ext:myDisplay ext:label \"Failing label\".
              }")
          (handle-update-unit:unwritten-data-error (e)
            (format t "Received expected error ~A" e)
            (return-from :no-error t)))
        (error 'simple-error :format-control "Expected triples not being written, but received no error."))
      (server:execute-query-for-context
       "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        INSERT DATA {
          ext:myDisplay ext:anotherThing \"Another thing\".
        }"))))
