(in-package #:handle-update-unit)

;; Conversion from bindings to matches and back

(defun binding-as-match (solution)
  "Constructs a match statement which corresponds to SOLUTION binding."
  (let ((type (jsown:val solution "type"))
        (value (jsown:val solution "value")))
    (flet ((make-string-literal ()
             (sparql-manipulation:make-string-literal value)))
      (cond ((string= type "uri")
             ;; uri
             (sparql-manipulation:iriref value))
            ((and (jsown:val-safe solution "xml:lang")
                  (string= type "literal"))
             ;; language typed strings
             (sparql-manipulation:make-nested-match
              `(ebnf::|RDFLiteral|
                      ,(make-string-literal)
                      ,(sparql-manipulation:make-token-match
                        'ebnf::|LANGTAG|
                        (concatenate 'string "@" (jsown:val solution "xml:lang"))))))
            ((and (jsown:val-safe solution "datatype")
                  (or (string= type "literal")
                      ;; virtuoso seems to emit typed-literal in some (all?) cases,
                      ;; sparql1.1 indicates to use literal
                      (string= type "typed-literal")))
             ;; datatype based strings
             (sparql-manipulation:make-nested-match
              `(ebnf::|RDFLiteral|
                      ,(make-string-literal) ;; TODO: if Virtuoso yields booleans as numbers, this would be one of the places to convert it
                      "^^"
                      ,(sparql-manipulation:make-iri (jsown:val solution "datatype")))))
            ((and (string= type "literal"))
             ;; handle string
             (sparql-manipulation:make-nested-match
              `(ebnf::|RDFLiteral| ,(make-string-literal))))
            ((and (string= type "bnode"))
             ;; support blank node
             (sparql-manipulation:make-nested-match
              `(ebnf::|BlankNode|
                      ,(sparql-manipulation:make-token-match
                        'ebnf::|BLANK_NODE_LABEL|
                        (concatenate 'string "_:" value)))))
            (t (error "Unknown solution to turn into match statement ~A" solution))))))

(defun match-as-binding (match)
  "Converts a MATCH statement to a binding.

This is the inverse of binding-as-match and can be used to create delta messages."
;;;; The supported match elements may have any of the following, plus
;;;; whatever binding-as-match may return.  At the point of writing
;;;; 20230323153815 this means we have the following options:
;;;;
;;;; ebnf::|VAR1| , ebnf::|VAR2|
;;;; , ebnf::|IRIREF| (CONS ebnf::|PNAME_LN| URI-STRING) , (CONS
;;;; ebnf::|PNAME_NS| URI-STRING) , ebnf::|RDFLiteral| ,
;;;; ebnf::|BooleanLiteral| , ebnf::|NumericLiteral|
  (if (consp match)
      ;; it must be something url-like, and the cdr is the uri
      (jsown:new-js ("type" "uri") ("value" (cdr match)))
      (case (sparql-parser:match-term match)
        (ebnf::|IRIREF| (jsown:new-js
                          ("type" "uri")
                          ("value" (sparql-manipulation:uri-unwrap-marks (terminal-match-string match)))))
        (ebnf::|RDFLiteral|
         ;; we can extract all cases by destructuring
         (destructuring-bind (ebnf-value-string &optional langtag-or-hathat hathat-iri)
             (sparql-parser:match-submatches match)
           ;; TODO: ensure hathatiri has an expandad iri in its primitive string when expanding if it is a prefixed name
           (let ((value-string (sparql-inspection:ebnf-string-real-string ebnf-value-string))
                 (langtag-or-hathat-string (and langtag-or-hathat
                                                (not hathat-iri)
                                                (terminal-match-string langtag-or-hathat)))
                 (hathat-iri-string (and hathat-iri
                                         (sparql-inspection:rdf-literal-datatype match))))
             (cond (hathat-iri (jsown:new-js
                                ("value" value-string)
                                ("datatype" hathat-iri-string)
                                ("type" "literal")))
                   (langtag-or-hathat ; must be langtag
                    (jsown:new-js
                      ("value" value-string)
                      ("xml:lang" (subseq langtag-or-hathat-string 1)) ; cut off @
                      ("type" "literal")))
                   (t (jsown:new-js
                        ("value" value-string)
                        ("type" "literal")))))))
        (ebnf::|BooleanLiteral| (jsown:new-js
                                  ("value" (sparql-parser:scanned-token-effective-string (sparql-inspection:first-found-scanned-token match))) ; TODO: convert to current interpretation of boolean, a limited set of values are realstic here and this is a good place to convert.
                                  ("datatype" "http://www.w3.org/2001/XMLSchema#boolean")
                                  ("type" "literal")))
        (ebnf::|NumericLiteral| (destructuring-bind (number-type . number-string)
                                    (sparql-inspection:ebnf-numeric-literal-extract-info match)
                                  (jsown:new-js
                                    ("value" number-string)
                                    ("datatype"
                                     (ecase number-type
                                       (:integer "http://www.w3.org/2001/XMLSchema#integer")
                                       (:decimal "http://www.w3.org/2001/XMLSchema#decimal")
                                       (:double "http://www.w3.org/2001/XMLSchema#double")))
                                    ("type" "literal"))))
        (ebnf::|VAR1| (error "Cannot make binding for variable"))
        (ebnf::|VAR2| (error "Cannot make binding for variable"))
        (otherwise (error "Unknown match ~A encountered to convert to binding." match)))))
