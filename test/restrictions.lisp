(defpackage :sparql-parser-test-restrictions
  (:use
   :common-lisp
   :fiveam
   :sparql-parser-test-integration
   :sparql-parser-test-utils))

(in-package :sparql-parser-test-restrictions)


(def-suite restrictions :in integration-tests)
(in-suite restrictions)

(db-test can-insert-some-random-content-test ()
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
         "boolean"))))

(db-test jack-cant-add-name-to-nonameorlabel-test ()
  (with-impersonation-for :jack
    (signals handle-update-unit:unwritten-data-error
      (server:execute-query-for-context
       "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        INSERT DATA {
          ext:myDisplay ext:name \"Failing name\".
        }"))))

(db-test jack-cant-add-label-to-nonameorlabel-test ()
  (with-impersonation-for :jack
    (signals handle-update-unit:unwritten-data-error
      (server:execute-query-for-context
       "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        INSERT DATA {
          ext:myDisplay ext:label \"Failing label\".
        }"))))

(db-test jack-can-add-other-predicates-to-nonameorlabel-test ()
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
         "boolean"))))


(db-test jack-can-delete-test ()
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
              "boolean")))))
