(defpackage :sparql-parser-test-updates
  (:use
   :common-lisp
   :fiveam
   :sparql-parser-test-integration
   :sparql-parser-test-utils))

(in-package :sparql-parser-test-updates)

(def-suite updates :in integration-tests)
(in-suite updates)

(db-test can-insert-a-push-update-test ()
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
                  }}"
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
                    "results" "bindings"))))))

(db-test whitespace-trimming-update-test ()
         (with-impersonation-for :jack
           (finishes
            (server:execute-query-for-context
             " PREFIX push: <http://mu.semte.ch/vocabularies/push/>
              PREFIX dct: <http://purl.org/dc/terms/>
              INSERT DATA {
                push:myUpdate a push:Update;
                  dct:title \"Receive delta without writing\".
              }"))))
