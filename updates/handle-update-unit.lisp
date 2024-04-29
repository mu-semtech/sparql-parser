(in-package #:handle-update-unit)

(defparameter *max-query-size-heuristic* 8000
  "Heuristing indicating roughly how many characters the body of quads may be in a single query.
Current implementation will try to query even if over this size.")

(defparameter *max-quads-per-query-heuristic* 100
  "Heuristing indicating roughly how many quads could be in a single query for insert or query.")

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
             (make-nested-match
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
             (make-nested-match
              `(ebnf::|RDFLiteral|
                      ,(make-string-literal) ;; TODO: if Virtuoso yields booleans as numbers, this would be one of the places to convert it
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
                          ("value" (sparql-manipulation:uri-unwrap-marks (detect-quads::primitive-match-string match)))))
        (ebnf::|RDFLiteral|
         ;; we can extract all cases by destructuring
         (destructuring-bind (ebnf-value-string &optional langtag-or-hathat hathat-iri)
             (sparql-parser:match-submatches match)
           ;; TODO: ensure hathatiri has an expandad iri in its primitive string when expanding if it is a prefixed name
           (let ((value-string (sparql-inspection:ebnf-string-real-string ebnf-value-string))
                 (langtag-or-hathat-string (and langtag-or-hathat
                                                (not hathat-iri)
                                                (detect-quads::primitive-match-string langtag-or-hathat)))
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
                                       (:double "http://www.w3.org/2001/XMLSchema#double"))))))
        (ebnf::|VAR1| (error "Cannot make binding for variable"))
        (ebnf::|VAR2| (error "Cannot make binding for variable"))
        (otherwise (error "Unknown match ~A encountered to convert to binding." match)))))

;;;; This is the entrypoint for executing update queries
;;;;
;;;; This file coordinates the detection of changed quads, informing any
;;;; necessary schedulers, sending out the updates to the triplestore,
;;;; as well as informing other units that an update has occurred
;;;; (namely the delta notifier and anything to clear caches).

(defun quad-object-as-match-term (quad-object)
  ;; NOTE: this was the original implementation TODO: Verify a
  ;; quad-object will always yield a iri or one of the following terms
  ;; and remove this old commented code or support other forms.
  ;; 'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|
  ;; 'ebnf::|iri|

  ;; (if (and (sparql-parser:match-p quad-object)
  ;;          (sparql-parser:match-term-p quad-object 'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|
  ;;                                      ;; 'ebnf::string_literal1 'ebnf::string_literal2 'ebnf::string_literal_long1 'ebnf::string_literal_long2
  ;;                                      ))
  ;;     quad-object
  ;;     (if nil
  ;;         ;; (and (listp quad-object)
  ;;         ;;      (find (car quad-object)
  ;;         ;;            (list 'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|)))
  ;;         quad-object
  ;;         (sparql-manipulation:make-iri (quad-term-uri quad-object))))
  (if (consp quad-object)
      (sparql-manipulation:make-iri (quad-term-uri quad-object)) ; in case of a prefixed uri
      (case (sparql-parser:match-term quad-object)
        (ebnf::|IRIREF| (make-nested-match `(ebnf::|iri| ,quad-object)))
        (otherwise quad-object))))

(defun make-quads-not-triples (quads)
  "Constructs a list of ebnf:|QuadsNotTriples| statements for the given set of quads."
  (labels ((make-triples-template-match (graphed-group)
             (when graphed-group
               (let* ((quad (first graphed-group))
                      (subject-iri (sparql-manipulation:make-iri (quad-term-uri (getf quad :subject))))
                      (predicate-iri (sparql-manipulation:make-iri (quad-term-uri (getf quad :predicate))))
                      (object-match (quad-object-as-match-term (getf quad :object))))
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
          for graph = (quad-term-uri (getf (first graphed-group) :graph))
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

(defun quad-equal-p (a b)
  "Yields truthy iff two quads are equal.  May provide false negatives but
should never provide false positives."
  ;; TODO: Is there a better package for this (perhaps in detect-quads package?)
  (and
   (every (lambda (key)
            (string= (quad-term-uri (getf a key))
                     (quad-term-uri (getf b key))))
          '(:graph :predicate :subject))
   (sparql-inspection:match-equal-p (quad-object-as-match-term (getf a :object))
                                    (quad-object-as-match-term (getf b :object)))))

(defun remove-database-value-overlaps (quads-to-delete existing-quads-to-insert)
  "Goes to the database to verify that quads-to-delete does not contain
values which will also be inserted through existing-quads-to-insert.
This allows us to remove cases where we did not detect two values as
being the same as per triplestore."
  (if (and quads-to-delete existing-quads-to-insert)
      (let ((possibly-overlapping-values-with-index
              (loop for delete-quad in quads-to-delete
                    for index from 0
                    append (loop for insert-quad in existing-quads-to-insert
                                 ;; we can detect uri's perfectly, only compare with those
                                 when (every (lambda (key) (string= (quad-term-uri (getf delete-quad key))
                                                                    (quad-term-uri (getf insert-quad key))))
                                               '(:predicate :subject :graph))
                                   collect (list index
                                                 delete-quad
                                                 insert-quad)))))
        (if possibly-overlapping-values-with-index
            (let ((possibly-overlapping-values-with-index-groups ; batch quads
                    ;; TODO: use length of only object part of match
                    (support:group-by-size-and-count
                     possibly-overlapping-values-with-index
                     :max-size *max-query-size-heuristic*
                     :max-count *max-quads-per-query-heuristic*
                     :item-size (lambda (list)
                                  (destructuring-bind (index delete-quad insert-quad)
                                      list
                                    (declare (ignore index))
                                    (+ 5
                                       (quad-as-string-size delete-quad)
                                       (quad-as-string-size insert-quad)))))))
              ;; collect results for each quad group
              (loop for possibly-overlapping-values-with-index-group in possibly-overlapping-values-with-index-groups
                    for object-values-with-index
                      = (mapcar (lambda (lst)
                                  (destructuring-bind (index delete-quad insert-quad) lst
                                    (list index
                                          (getf delete-quad :object)
                                          (getf insert-quad :object))))
                                possibly-overlapping-values-with-index-group)
                    for ast = (sparql-parser:make-sparql-ast :top-node
                                                             (query-to-detect-overlapping-values object-values-with-index)
                                                             :string
                                                             sparql-parser:*scanning-string*)
                    for query-result = (client:bindings (client:query (sparql-generator:write-when-valid ast)
                                                                      :send-to-single t))
                    for quad-indexes-to-delete
                      = (delete-duplicates
                         (loop for binding in query-result
                               collect (parse-integer (jsown:filter binding "index" "value"))))
                    append (loop for quad in quads-to-delete
                                 for index from 0 ; because looping over full quads-to-delete list
                                 unless (find index quad-indexes-to-delete :test #'=)
                                   collect quad)))
            quads-to-delete))
      quads-to-delete))

(defun query-to-detect-overlapping-values (possibly-overlapping-values-with-index)
  "Constructs a query which detects values which the triplestore deems to be the same and yields the corresponding index.

POSSIBLY-OVERLAPPING-INDEXES is a list containing lists of IDX A B in
which the index will be returned IFF the database thinks A and B are the
same value."
  ;; Emits a query such as:
  ;;
  ;; SELECT ?index WHERE {
  ;;   VALUES (?index ?a ?b) {
  ;;     (0 "obj2" "obj1")
  ;;     (1 "obj2" "obj2")
  ;;   }
  ;    FILTER (?a = ?b)
  ;; }

  (flet ((make-inline-value-blocks ()
           (loop for (index a b) in possibly-overlapping-values-with-index
                 append
                 `("("
                   ,(sparql-manipulation:make-match-up-to-scanned-token
                     :string (format nil "~A" index)
                     :match-list '(ebnf::|DataBlockValue| ebnf::|NumericLiteral| ebnf::|NumericLiteralUnsigned| ebnf::|INTEGER|))
                   (ebnf::|DataBlockValue| ,(quad-object-as-match-term a))
                   (ebnf::|DataBlockValue| ,(quad-object-as-match-term b))
                   ")"))))
    (make-nested-match
     `(ebnf::|QueryUnit|
             (ebnf::|Query|
                    ebnf::|Prologue|
                    (ebnf::|SelectQuery|
                           (ebnf::|SelectClause|
                                  "SELECT"
                                  ,(sparql-manipulation:make-var "?index"))
                           (ebnf::|WhereClause|
                                  "WHERE"
                                  (ebnf::|GroupGraphPattern|
                                         "{"
                                         (ebnf::|GroupGraphPatternSub|
                                                (ebnf::|GraphPatternNotTriples|
                                                       (ebnf::|InlineData|
                                                              "VALUES"
                                                              (ebnf::|DataBlock|
                                                                     (ebnf::|InlineDataFull|
                                                                            "("
                                                                            ,(sparql-manipulation:make-var "?index")
                                                                            ,(sparql-manipulation:make-var "?a")
                                                                            ,(sparql-manipulation:make-var "?b")
                                                                            ")"
                                                                            "{"
                                                                            ,@(make-inline-value-blocks)
                                                                            "}"))))
                                                (ebnf::|GraphPatternNotTriples|
                                                       (ebnf::|Filter|
                                                              "FILTER"
                                                              (ebnf::|Constraint|
                                                                     (ebnf::|BrackettedExpression|
                                                                           "("
                                                                           (ebnf::|Expression|
                                                                                  (ebnf::|ConditionalOrExpression|
                                                                                         (ebnf::|ConditionalAndExpression|
                                                                                                (ebnf::|ValueLogical|
                                                                                                       (ebnf::|RelationalExpression|
                                                                                                              (ebnf::|NumericExpression|
                                                                                                                     (ebnf::|AdditiveExpression|
                                                                                                                            (ebnf::|MultiplicativeExpression|
                                                                                                                                   (ebnf::|UnaryExpression|
                                                                                                                                          (ebnf::|PrimaryExpression|
                                                                                                                                                 ,(sparql-manipulation:make-var "?a"))))))
                                                                                                              "="
                                                                                                              (ebnf::|NumericExpression|
                                                                                                                     (ebnf::|AdditiveExpression|
                                                                                                                            (ebnf::|MultiplicativeExpression|
                                                                                                                                   (ebnf::|UnaryExpression|
                                                                                                                                          (ebnf::|PrimaryExpression|
                                                                                                                                                 ,(sparql-manipulation:make-var "?b")))))))))))
                                                                           ")")))))
                                         "}"))
                           ebnf::|SolutionModifier|)
                    ebnf::|ValuesClause|)))))

(defun detect-effective-changes (&key delete-quads insert-quads)
  "Calculates the quads that will effectively be written to the triplestore
based on the supplied arguments and the state in the triplestore.

  Yields (values effective-deletes effective-inserts) which are to be
  executed elsewhere."
  ;; possible cases
  ;;
  ;; |------------------------+---+---+---+---+---+---+---+---|
  ;; | Start state            | x | x | x | x |   |   |   |   |
  ;; | To delete              | x | x |   |   | x | x |   |   |
  ;; | To insert              | x |   | x |   | x |   | x |   |
  ;; |------------------------+---+---+---+---+---+---+---+---|
  ;; | Existing to delete     | x | x |   |   |   |   |   |   |
  ;; | Non-existing to delete |   |   |   |   | x | x |   |   |
  ;; | Existing to insert     | x |   | x |   |   |   |   |   |
  ;; | Non-existing to insert |   |   |   |   | x |   | x |   |
  ;; |------------------------+---+---+---+---+---+---+---+---|
  ;; | Effective delete       |   | x |   |   |   |   |   |   |
  ;; | Effective insert       |   |   |   |   | x |   | x |   |
  (multiple-value-bind (existing-quads-to-delete non-existing-quads-to-delete)
      (find-existing-quads delete-quads)
    (declare (ignore non-existing-quads-to-delete))
    (multiple-value-bind (existing-quads-to-insert non-existing-quads-to-insert)
        (find-existing-quads insert-quads)
      (let ((quads-to-delete (set-difference existing-quads-to-delete existing-quads-to-insert
                                             :test #'quad-equal-p))
            (quads-to-insert non-existing-quads-to-insert))
        (let ((quads-to-delete-with-database-value-check
                ;; the database may find some values the same even
                ;; though the RDF spec considers them different which
                ;; may create false deletes. eg: 12e5 vs 1.2e6
                (remove-database-value-overlaps quads-to-delete existing-quads-to-insert)))
          (values quads-to-delete-with-database-value-check
                  quads-to-insert))))))

(defun query-to-detect-existing-quad-indexes (quads)
  "Constructs a query which detects quads exist in the triplestore of quads.  Yields the indexes of those quads."
  ;; Emits a query such as:
  ;;
  ;; SELECT ?index WHERE {
  ;;   VALUES (?index ?graph ?s ?p ?o) {
  ;;     (0 <graph> <sub1> <pred1> "obj1")
  ;;     (1 <graph> <sub2> <pred2> "obj2")
  ;;   }
  ;;   GRAPH ?graph {
  ;;     ?s ?p ?o.
  ;;   }
  ;; }
  (flet ((make-inline-data-blocks (quads)
           (loop for quad in quads
                 for index from 0
                 append
                 `("("
                   ,(sparql-manipulation:make-match-up-to-scanned-token
                     :string (format nil "~A" index)
                     :match-list '(ebnf::|DataBlockValue| ebnf::|NumericLiteral| ebnf::|NumericLiteralUnsigned| ebnf::|INTEGER|))
                   (ebnf::|DataBlockValue| ,(sparql-manipulation:make-iri (quad-term-uri (getf quad :graph))))
                   (ebnf::|DataBlockValue| ,(sparql-manipulation:make-iri (quad-term-uri (getf quad :subject))))
                   (ebnf::|DataBlockValue| ,(sparql-manipulation:make-iri (quad-term-uri (getf quad :predicate))))
                   (ebnf::|DataBlockValue| ,(quad-object-as-match-term (getf quad :object)))
                   ")"))))
    (make-nested-match
     `(ebnf::|QueryUnit|
             (ebnf::|Query|
                    ebnf::|Prologue|
                    (ebnf::|SelectQuery|
                           (ebnf::|SelectClause|
                                  "SELECT"
                                  ,(sparql-manipulation:make-var "?index"))
                           (ebnf::|WhereClause|
                                  "WHERE"
                                  (ebnf::|GroupGraphPattern|
                                         "{"
                                         (ebnf::|GroupGraphPatternSub|
                                                (ebnf::|GraphPatternNotTriples|
                                                       (ebnf::|InlineData|
                                                              "VALUES"
                                                              (ebnf::|DataBlock|
                                                                     (ebnf::|InlineDataFull|
                                                                            "("
                                                                            ,(sparql-manipulation:make-var "?index")
                                                                            ,(sparql-manipulation:make-var "?graph")
                                                                            ,(sparql-manipulation:make-var "?s")
                                                                            ,(sparql-manipulation:make-var "?p")
                                                                            ,(sparql-manipulation:make-var "?o")
                                                                            ")"
                                                                            "{"
                                                                            ,@(make-inline-data-blocks quads)
                                                                            "}"))))
                                                (ebnf::|GraphPatternNotTriples|
                                                       (ebnf::|GraphGraphPattern|
                                                              "GRAPH"
                                                              (ebnf::|VarOrIri| ,(sparql-manipulation:make-var "?graph"))
                                                              (ebnf::|GroupGraphPattern|
                                                                     "{"
                                                                     (ebnf::|GroupGraphPatternSub|
                                                                            (ebnf::|TriplesBlock|
                                                                                   (ebnf::|TriplesSameSubjectPath|
                                                                                          (ebnf::|VarOrTerm|
                                                                                                 ,(sparql-manipulation:make-var "?s"))
                                                                                          (ebnf::|PropertyListPathNotEmpty|
                                                                                                 (ebnf::|VerbSimple|
                                                                                                        ,(sparql-manipulation:make-var "?p"))
                                                                                                 (ebnf::|ObjectListPath|
                                                                                                        (ebnf::|ObjectPath|
                                                                                                               (ebnf::|GraphNodePath|
                                                                                                                      (ebnf::|VarOrTerm|
                                                                                                                             ,(sparql-manipulation:make-var "?o")))))))
                                                                                   "."))
                                                                     "}"))))
                                         "}"))
                           ebnf::|SolutionModifier|)
                    ebnf::|ValuesClause|)))))

(defun quad-as-string-size (quad)
  "Converts QUAD into an estimate for the size of its representation as a string in a sparql query."
  (+
   (loop for part in '(:subject :predicate :object :graph)
         for value = (getf quad part)
         if (consp value)
           sum (length (cdr value))
         else
           sum (length (sparql-generator:write-valid-match value)))
   4))

(defun find-existing-quads (quads)
  "Searches the triplestore and yields the quads which exist.

  Return (VALUES existing-quads non-existing-quads)"
  (let (existing-quads
        non-existing-quads
        (quad-groups (support:group-by-size-and-count
                      quads
                      :max-size *max-query-size-heuristic*
                      :max-count *max-quads-per-query-heuristic*
                      :item-size #'quad-as-string-size)))
    (dolist (quad-group quad-groups)
      (let* ((ast (sparql-parser:make-sparql-ast :top-node (query-to-detect-existing-quad-indexes quad-group)
                                                 :string sparql-parser:*scanning-string*))
             (query-result (client:bindings (client:query (sparql-generator:write-when-valid ast)
                                                          :send-to-single t)))
             (existing-quad-indexes (loop for binding in query-result
                                          collect (parse-integer (jsown:filter binding "index" "value")))))
        (loop for quad in quad-group
              for index from 0
              if (find index existing-quad-indexes)
                do (push quad existing-quads)
              else
                do (push quad non-existing-quads))))
    (values existing-quads non-existing-quads)))

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

(defun alter-quad-to-string-file-uris (quad)
  "Converts the object portion of a quad into a file uri if necessary."
  (let ((object (getf quad :object)))
    (when (and (not (consp object)) ; it's certainly a uri
               (sparql-inspection:ebnf-simple-string-p object)) ; it's a string
      (multiple-value-bind (uri-replacement uri-replacement-p)
          (support:maybe-string-to-uri (sparql-inspection:ebnf-string-real-string object))
        (when uri-replacement-p ; the string has been replaced by a URI
          (setf (getf quad :object) (sparql-manipulation:iriref uri-replacement))))))
  quad)

(defun alter-quad-string-file-uri-to-string (quad)
  (let ((object (getf quad :object)))
    (when (and (not (consp object))
               (eq (sparql-parser:match-term object) 'ebnf::|IRIREF|))
      (let ((uri-string (support:-> object
                          (sparql-inspection:first-found-scanned-token)
                          (sparql-parser:scanned-token-effective-string)
                          (sparql-manipulation:uri-unwrap-marks))))
        (multiple-value-bind (string-replacement string-replacement-p)
            (support:maybe-uri-to-string uri-string)
          (when string-replacement-p
            (setf (getf quad :object)
                  (sparql-manipulation:make-rdfliteral string-replacement)))))))
  quad)

(defun handle-sparql-update-unit (update-unit)
  "Handles the processing of an update-unit EBNF."
  ;; TODO: add conditional validation ensuring all triples were written somewhere
  ;; TODO: add validation ensuring all triples were written to a readable location
  (flet ((execute-and-dispatch-changes (&key delete-quads insert-quads)
           (let* (;; once we have the effective content to alter, we can safely create files from long strings
                  (delete-quads-with-string-file-uris (mapcar #'alter-quad-to-string-file-uris delete-quads))
                  (insert-quads-with-string-file-uris (mapcar #'alter-quad-to-string-file-uris insert-quads))
                  ;; we can deduplicate everything
                  (dedup-delete-quads (remove-duplicates delete-quads-with-string-file-uris :test #'quad-equal-p))
                  (dedup-insert-quads (remove-duplicates insert-quads-with-string-file-uris :test #'quad-equal-p)))
             (multiple-value-bind (effective-deletes effective-inserts)
                 (detect-effective-changes :delete-quads
                                           dedup-delete-quads
                                           :insert-quads
                                           dedup-insert-quads)
               (dolist (group (support:group-by-size-and-count
                               (nconc (mapcar (lambda (a) (cons :delete a)) effective-deletes)
                                      (mapcar (lambda (a) (cons :insert a)) effective-inserts))
                               :max-size *max-query-size-heuristic*
                               :max-count *max-quads-per-query-heuristic*
                               :item-size (alexandria:compose #'quad-as-string-size #'cdr)))
                 (client:query (query-for-quad-changes :delete-quads (loop for (key . quad) in group
                                                                           when (eq key :delete)
                                                                             collect quad)
                                                       :insert-quads (loop for (key . quad) in group
                                                                           when (eq key :insert)
                                                                             collect quad))
                               :send-to-single nil))
               (type-cache:update-known-types :deletes effective-deletes :inserts effective-inserts)
               (delta-messenger:delta-notify
                :deletes (mapcar #'alter-quad-string-file-uri-to-string dedup-delete-quads)
                :inserts (mapcar #'alter-quad-string-file-uri-to-string dedup-insert-quads)
                :effective-deletes (mapcar #'alter-quad-string-file-uri-to-string effective-deletes)
                :effective-inserts (mapcar #'alter-quad-string-file-uri-to-string effective-inserts))))))
   (dolist (operation (detect-quads-processing-handlers:|UpdateUnit| update-unit))
     ;; (format t "~&Treating operation ~A~%" operation)
     ;; (break "Got operation ~A" operation)
     (case (operation-type operation)
       (:insert-triples
        (let* ((data (operation-data operation))
               (quads (acl:dispatch-quads data)))
          (assert-no-variables-in-quads data)
          (execute-and-dispatch-changes :insert-quads quads)))
       (:delete-triples
        (let* ((data (operation-data operation))
               (quads (acl:dispatch-quads data)))
          (assert-no-variables-in-quads data)
          (execute-and-dispatch-changes :delete-quads quads)))
       (:modify
        ;; TODO: handle WITH iriref which should be removed for non sudo queries
        (let ((insert-patterns (operation-data-subfield operation :insert-patterns))
              (delete-patterns (operation-data-subfield operation :delete-patterns))
              (bindings (client:batch-create-full-solution-for-select-query
                         (operation-data-subfield operation :query)
                         :for :modify :usage :read)))
          (if bindings
              (let ((inserts (acl:dispatch-quads (filled-in-patterns insert-patterns bindings)))
                    (deletes (acl:dispatch-quads (filled-in-patterns delete-patterns bindings))))
                (execute-and-dispatch-changes :delete-quads deletes :insert-quads inserts))
              ;; Following is subject to change.  Nothing should depend
              ;; on the resulting output, sending out a json body could
              ;; be assumed, might as well send something sensible.
              "{ \"head\": { \"link\": [], \"vars\": [\"callret-0\"] }, \"results\": { \"distinct\": false, \"ordered\": true, \"bindings\": [ { \"callret-0\": { \"type\": \"literal\", \"value\": \"Insert into <http://www.openlinksw.com/schemas/virtrdf#DefaultSparul11Target>, 0 triples -- nothing to do\" }} ] } }")))))))

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
