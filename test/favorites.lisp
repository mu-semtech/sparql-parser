(defpackage :sparql-parser-test-favorites
  (:use
   :common-lisp
   :fiveam
   :sparql-parser-test-integration
   :sparql-parser-test-utils))

(in-package :sparql-parser-test-favorites)

(def-suite favorites :in integration-tests)
(in-suite favorites)

(db-test jack-can-add-favorite-test ()
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
         "boolean"))))

(db-test jack-can-add-conditional-favorite-authors-test ()
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
           "PREFIX authors: <http://example.com/authors/>
            PREFIX favorites: <http://mu.semte.ch/favorites/>
            PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
            ASK {
              favorites:me ext:hasFavoriteAuthor authors:steven.
              favorites:me ext:hasFavoriteAuthor authors:peter.
            }"))
         "boolean"))))

(db-test jack-cant-add-books-as-favorite-author-test ()
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
        }"))))


(db-test jack-can-ask-for-favorite-authors-test ()
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
         "boolean"))))

(db-test jack-can-describe-favorite-authors-test ()
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
        }"))))

(db-test jack-can-execute-delete-where-and-insert-data-in-one-query-test ()
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
         "boolean"))))
