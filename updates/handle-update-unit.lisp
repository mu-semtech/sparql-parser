(in-package #:handle-update-unit)

(defun make-nested-match (specification)
  "Constructs a nested match through an abbreviated interface.

SPECIFICATION is either (TERM &REST CHILDREN) or TERM.

- When term is a symbol, a match is constructed with the same symbol.
- When term is a string, a word match is constructed.
- Any nil children are ignored
- Any other resource is consumed verbatim.

Children is a list that will be mapped through nested match under the
same logic to construct the submatches."
  ;; TODO: move this to supporting code and update detect-quads.lisp for it
  (flet ((make-match (term)
           (cond 
             ((symbolp term)
              (make-match :term term
                          :rule nil
                          :submatches nil))
             ((stringp term)
              (sparql-manipulation:make-word-match term))
             (t term))))
   (if (listp specification)
       (destructuring-bind (term &rest children)
           specification
         (let ((match (make-match term)))
           (when children
             (setf (sparql-parser:match-submatches match)
                   (mapcar #'make-nested-match
                           (remove-if-not #'identity children))))
           match))
       (make-match specification))))

(defun sparql-escape-string (string)
  "Generate an escaped SPARQL string."
  (concatenate 'string
               "\""
               (cl-ppcre:regex-replace-all "([\"\\\\])" string "\\\\\\1")
               "\""))

(defun make-token-match (term string)
  "Makes a token match for the given string.  This is a MATCH which has a SCANNED-TOKEN as a match."
  `(,term
    ,(sparql-parser:make-scanned-token
      :start 0 :end 0
      :string string
      :token term)))

(defun binding-as-match (solution)
  "Constructs a match statement which corresponds to SOLUTION binding."
  (let ((type (jsown:val solution "type"))
        (value (jsown:val solution "value")))
    (flet ((make-string-literal ()
             `(ebnf::|String|
                     ,(make-token-match 'ebnf::|STRING_LITERAL_LONG2|
                                        (sparql-escape-string value)))))
      (cond ((string= type "uri")
             ;; uri
             (sparql-manipulation:iriref value))
            ((and (jsown:val-safe solution "xml:lang")
                  (string= type "literal"))
             ;; language typed strings
             (make-nested-match
              `(ebnf::|RDFLiteral|
                      ,(make-string-literal)
                      ,(make-token-match 'ebnf::|LANGTAG|
                                         (concatenate 'string "@" (jsown:val solution "xml:lang"))))))
            ((and (jsown:val-safe solution "datatype")
                  (or (string= type "literal")
                      ;; virtuoso seems to emit typed-literal in some (all?) cases,
                      ;; sparql1.1 indicates to use literal
                      (string= type "typed-literal")))
             ;; datatype based strings
             (make-nested-match
              `(ebnf::|RDFLiteral|
                      ,(make-string-literal)
                      "^^"
                      ,(sparql-manipulation:make-iri (jsown:val solution "datatype")))))
            ((and (string= type "literal"))
             ;; handle string
             (make-nested-match
              `(ebnf::|RDFLiteral| ,(make-string-literal))))
            ((and (string= type "bnode"))
             ;; support blank node
             (make-nested-match
              `(ebnf::|BlankNode|
                      ,(make-token-match 'ebnf::|BLANK_NODE_LABEL|
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
                          ("value" (sparql-manipulation:uri-unwrap-marks (detect-quads::primitive-match-string match)))))
        (ebnf::|RDFLiteral|
         ;; we can extract all cases by destructuring
         (destructuring-bind (ebnf-value-string &optional langtag-or-hathat hathat-iri)
             (sparql-parser:match-submatches match)
           ;; TODO: ensure hathatiri has an expandad iri in its primitive string when expanding if it is a prefixed name
           (let ((value-string (detect-quads::primitive-match-string (first (sparql-parser:match-submatches ebnf-value-string))))
                 (langtag-or-hathat-string (and langtag-or-hathat
                                                (not hathat-iri)
                                                (detect-quads::primitive-match-string (first (sparql-parser:match-submatches langtag-or-hathat)))))
                 (hathat-iri-string (and hathat-iri
                                         (sparql-parser:scanned-token-effective-string
                                          (detect-quads:first-found-scanned-token
                                           hathat-iri)))))
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
                                  ("value" (detect-quads::primitive-match-string match)) ; TODO: convert to current interpretation of boolean, a limited set of values are realstic here and this is a good place to convert.
                                  ("datatype" "http://www.w3.org/2001/XMLSchema#boolean")
                                  ("type" "literal")))
        (ebnf::|NumericLiteral| (jsown:new-js
                                  ("value" (detect-quads::primitive-match-string match))
                                  ("datatype" "http://www.w3.org/2001/XMLSchema#number")))
        (ebnf::|VAR1| (error "Cannot make binding for variable"))
        (ebnf::|VAR2| (error "Cannot make binding for variable"))
        (otherwise (error "Unknown match ~A encountered to convert to binding." match)))))

;;;; This is the entrypoint for executing update queries
;;;;
;;;; This file coordinates the detection of changed quads, informing any
;;;; necessary schedulers, sending out the updates to the triplestore,
;;;; as well as informing other units that an update has occurred
;;;; (namely the delta notifier and anything to clear caches).

(defun make-quads-not-triples (quads)
  "Constructs a list of ebnf:|QuadsNotTriples| statements for the given set of quads."
  (labels ((make-triples-template-match (graphed-group)
             (when graphed-group
               (let* ((quad (first graphed-group))
                      (subject-iri (sparql-manipulation:make-iri (quad-term-uri (getf quad :subject))))
                      (predicate-iri (sparql-manipulation:make-iri (quad-term-uri (getf quad :predicate))))
                      (quad-object (getf quad :object))
                      (object-match ;; TODO: support other types of resources than IRIs
                        (if (and (sparql-parser:match-p quad-object)
                                 (sparql-parser:match-term-p quad-object 'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|
                                                             ;; 'ebnf::string_literal1 'ebnf::string_literal2 'ebnf::string_literal_long1 'ebnf::string_literal_long2
                                                             ))
                            quad-object
                            (if nil
                                ;; (and (listp quad-object)
                                ;;      (find (car quad-object)
                                ;;            (list 'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|)))
                                quad-object
                                (sparql-manipulation:make-iri (quad-term-uri quad-object))))))
                 (make-nested-match
                  `(ebnf::|TriplesTemplate|
                          (ebnf::|TriplesSameSubject|
                                 (ebnf::|VarOrTerm| (ebnf::|GraphTerm| ,subject-iri))
                                 (ebnf::|PropertyListNotEmpty|
                                        (ebnf::|Verb|
                                               (ebnf::|VarOrIri| ,predicate-iri))
                                        (ebnf::|ObjectList|
                                               (ebnf::|Object|
                                                      (ebnf::|GraphNode|
                                                             (ebnf::|VarOrTerm|
                                                                    (ebnf::|GraphTerm|
                                                                           ,object-match)))))))
                          "."
                          ,(make-triples-template-match (rest graphed-group))))))))
    (loop for graphed-group
            in (support:group-by quads #'string=
                                 :key (lambda (quad)
                                        (quad-term-uri (getf quad :graph))))
          for graph = (quad-term-uri (getf (first quads) :graph))
          collect
          (make-nested-match
           `(ebnf::|QuadsNotTriples|
                   "GRAPH"
                   (ebnf::|VarOrIri| ,(sparql-manipulation:make-iri graph))
                   "{"
                   ,(make-triples-template-match graphed-group)
                   "}")))))

(defun insert-data-query-from-quads-not-triples (quads-not-triples) 
 "Constructs an ebnf:|InsertData| query to store the list of quads-not-triples."
  (let ((match (make-nested-match
                `(ebnf::|UpdateUnit|
                        (ebnf::|Update|
                               ebnf::|Prologue|
                               (ebnf::|Update1|
                                      (ebnf::|InsertData|
                                             "INSERT DATA"
                                             (ebnf::|QuadData|
                                                    "{"
                                                    (ebnf::|Quads| ,@quads-not-triples)
                                                    "}"))))))))
    (sparql-parser:make-sparql-ast :top-node match :string sparql-parser:*scanning-string*)))

(defun delete-data-query-from-quads-not-triples (quads-not-triples)
  "Constructs an ebnf:|InsertData| query to store the list of quads-not-triples."
  (let ((match (make-nested-match
                `(ebnf::|UpdateUnit|
                        (ebnf::|Update|
                               ebnf::|Prologue|
                               (ebnf::|Update1|
                                      (ebnf::|DeleteData|
                                             "DELETE DATA"
                                             (ebnf::|QuadData|
                                                    "{"
                                                    (ebnf::|Quads| ,@quads-not-triples)
                                                    "}"))))))))
    (sparql-parser:make-sparql-ast :top-node match :string sparql-parser:*scanning-string*)))

(defun insert-data-query-for-quads (quads)
  (let* ((quads-not-triples (make-quads-not-triples quads))
         (query (insert-data-query-from-quads-not-triples quads-not-triples))
         (query-string (sparql-generator:write-when-valid query)))
    (coerce query-string #-be-cautious 'base-string #+be-cautious 'string)))

(defun delete-data-query-for-quads (quads)
  (let* ((quads-not-triples (make-quads-not-triples quads))
         (query (delete-data-query-from-quads-not-triples quads-not-triples))
         (query-string (sparql-generator:write-when-valid query)))
    query-string))

(defun make-combined-delete-insert-data-query (quads-to-delete quads-to-insert)
  "Constructs a SPARQL query for the combination of QUADS-TO-DELETE and QUADS-TO-INSERT."
  (let ((delete-quads-not-triples (make-quads-not-triples quads-to-delete))
        (insert-quads-not-triples (make-quads-not-triples quads-to-insert)))
    (let ((match (make-nested-match
                  `(ebnf::|UpdateUnit|
                          (ebnf::|Update|
                                 ebnf::|Prologue|
                                 (ebnf::|Update1|
                                        (ebnf::|DeleteData|
                                               "DELETE DATA"
                                               (ebnf::|QuadData|
                                                      "{"
                                                      (ebnf::|Quads| ,@delete-quads-not-triples)
                                                      "}")))
                                 ";"
                                 (ebnf::|Update|
                                        ebnf::|Prologue|
                                        (ebnf::|Update1|
                                               (ebnf::|InsertData|
                                                      "INSERT DATA"
                                                      (ebnf::|QuadData|
                                                             "{"
                                                             (ebnf::|Quads| ,@insert-quads-not-triples)
                                                             "}")))))))))
      (sparql-generator:write-when-valid
       (sparql-parser:make-sparql-ast :top-node match :string sparql-parser:*scanning-string*)))))

(defun query-for-quad-changes (&key delete-quads insert-quads)
  "Constructs a query for quad changes based on available delete quads and insert quads."
  (cond
    ((null delete-quads)
     (insert-data-query-for-quads insert-quads))
    ((null insert-quads)
     (delete-data-query-for-quads delete-quads))
    (t (make-combined-delete-insert-data-query delete-quads insert-quads))))

(defun filled-in-patterns (patterns bindings)
  "Creates a set of QUADS for the given patterns and bindings.

Any pattern which has no variables will be returned as is.  Any pattern
with bindings will be filled in for each discovered binding.  If any
variables are missing this will not lead to a pattern."
  (flet ((pattern-has-variables (pattern)
           (loop for (place match) on pattern by #'cddr
                 when (and (sparql-parser:match-p match)
                           (sparql-parser:match-term-p match 'ebnf::|VAR1| 'ebnf::|VAR2|))
                   return t))
         (fill-in-pattern (pattern bindings)
           (loop for (place match) on pattern by #'cddr
                 if (and (sparql-parser:match-p match)
                         (sparql-parser:match-term-p match 'ebnf::|VAR1| 'ebnf::|VAR2|)
                         (jsown:keyp bindings (subseq (sparql-parser:terminal-match-string match) 1))) ; binding contains key (OPTIONAL in queries)
                   append (list place
                                (let ((solution (jsown:val bindings (subseq (sparql-parser:terminal-match-string match) 1))))
                                  (if solution
                                      (binding-as-match solution)
                                      match)))
                 else
                   append (list place match))))
   (let* ((patterns-without-bindings (remove-if #'pattern-has-variables patterns))
          (patterns-with-bindings (set-difference patterns patterns-without-bindings :test #'eq)))
     (concatenate
      'list
      patterns-without-bindings        ; pattern without binding is quad
      (loop for binding in bindings
            append
            (loop for pattern in patterns-with-bindings
                  for filled-in-pattern = (fill-in-pattern pattern binding)
                  unless (pattern-has-variables filled-in-pattern)
                    collect filled-in-pattern))))))

(defun handle-sparql-update-unit (update-unit)
  "Handles the processing of an update-unit EBNF."
  ;; TODO: verify insert-triples and delete-triples don't contain any more variables
  ;; TODO: execute where block if it exists
  ;; TODO: add conditional validation ensuring all triples were written somewhere
  ;; TODO: add validation ensuring all triples were written to a readable location
  (dolist (operation (detect-quads-processing-handlers:|UpdateUnit| update-unit))
    ;; (format t "~&Treating operation ~A~%" operation)
    ;; (break "Got operation ~A" operation)
    (case (operation-type operation)
      (:insert-triples
       (let* ((data (operation-data operation))
              (quads (acl:dispatch-quads data)))
         (assert-no-variables-in-quads data)
         (client:query (query-for-quad-changes :insert-quads quads))
         (type-cache:update-known-types :inserts quads)
         (delta-messenger:delta-notify :inserts quads)))
      (:delete-triples
       (let* ((data (operation-data operation))
              (quads (acl:dispatch-quads data)))
         (assert-no-variables-in-quads data)
         (client:query (query-for-quad-changes :delete-quads quads))
         (type-cache:update-known-types :deletes quads)
         (delta-messenger:delta-notify :deletes quads)))
      (:modify
       ;; TODO: handle WITH iriref which should be removed for non sudo queries
       (let ((insert-patterns (operation-data-subfield operation :insert-patterns))
             (delete-patterns (operation-data-subfield operation :delete-patterns)))
         ;; TODO: verify the logic that we can execute insert-patterns
         ;; and delete-patterns in batches because anything static will
         ;; be inserted and deleted in the batch too.
         ;; (break "Going to treat the select query")
         (client:batch-map-solutions-for-select-query ((operation-data-subfield operation :query) :for :modify :usage :read) (bindings)
           (let ((inserts (acl:dispatch-quads (filled-in-patterns insert-patterns bindings)))
                 (deletes (acl:dispatch-quads (filled-in-patterns delete-patterns bindings))))
             (client:query (query-for-quad-changes :delete-quads deletes :insert-quads inserts))
             (type-cache:update-known-types :deletes deletes :inserts inserts)
             (delta-messenger:delta-notify :deletes deletes :inserts inserts))))))))

(defun unfold-prefixed-quads (quads)
  "Unfolds the prefixed quads (represented by a CONS cell) into a match
which houses a primitive IRI."
  (flet ((unfold (value)
           (sparql-parser::make-match
            :term 'ebnf::|IRIREF|
            :submatches (list (sparql-parser::make-scanned-token
                               :start 0 :end 0
                               :string (cdr value)
                               :token 'ebnf::|IRIREF|)))))
    (loop for quad in quads
          collect (loop for (k v) on quad by #'cddr
                        if (consp v)
                          append (list k (unfold v))
                        else
                          append (list k v)))))

(defun assert-no-variables-in-quads (quads)
  "Asserts there are no variables in this quad."
  (dolist (quad quads)
    (loop for (k v) on quad by #'cddr
          when (sparql-parser:match-p v) ;; ignore nil graph and cons with expanded prefix
          do
             (assert (not (find (sparql-parser:match-term v) '(ebnf::|VAR1| ebnf::|VAR2|) :test #'eq))))))
