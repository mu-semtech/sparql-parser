(defpackage :sparql-parser-test-authors
  (:use
   :common-lisp
   :fiveam
   :sparql-parser-test-utils
   :sparql-parser-test-integration))

(in-package :sparql-parser-test-authors)


(def-suite authors :in integration-tests)
(in-suite authors)

(db-test joll-can-add-authors-test ()
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
         "boolean"))))

(db-test joll-can-add-authors2-test ()
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
         "boolean"))))

(db-test joll-can-add-extra-book-for-author-test ()
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
         "boolean"))))

(db-test joll-can-add-extra-author-for-book-test ()
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
         "boolean"))))
