(defpackage :sparql-parser-test-books
  (:use
   :common-lisp
   :fiveam
   :sparql-parser-test-integration
   :sparql-parser-test-utils))

(in-package :sparql-parser-test-books)


(def-suite books :in integration-tests)
(in-suite books)

(db-test joll-can-write-a-book-title-with-the-right-uri-and-no-type-test ()
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
         "boolean"))))

(db-test changes-contain-only-the-data-that-was-actually-changed-test ()
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
         "boolean"))))


(db-test reinserting-long-content-does-not-duplicate-abbreviation-test ()
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
                 "value")))))))

(db-test joll-can-collapse-multiple-tiles-via-delete-insert-where-test ()
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
         "boolean"))))


(db-test we-can-delete-the-types-test ()
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
              "boolean")))))


(db-test we-can-have-an-empty-construct-where-test ()
  (with-impersonation-for :joll
    (finishes
      (server:execute-query-for-context
       "CONSTRUCT { } WHERE { }"))))

;; ;; TODO: is this ok?
(db-test inserting-the-uuid-will-just-insert-the-uuid-test ()
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

      (is (jsown:val (jsown:val binding "uuid") "value") "123"))))


(db-test geo-sparql-test ()
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
        (is (identity t)))))
