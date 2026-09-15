(defpackage :sparql-parser-test-integration
  (:use :common-lisp :fiveam)
  (:export
   #:integration-tests
   #:run-tests))

(in-package :sparql-parser-test-integration)

(def-suite integration-tests)

;;;; Boot up a container using:
;;;; docker run --name virtuoso -p 8891:8890 -e SPARQL_UPDATE=true -e "DEFAULT_GRAPH=http://mu.semte.ch/application" redpencil/virtuoso:1.2.0-rc.1; dr rm virtuoso
(defun run-tests ()
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

  (run! 'integration-tests))
