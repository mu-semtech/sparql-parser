(in-package #:handle-update-unit)

;; Converting into CONSTRUCT:
;;
;; There are multiple cases for which we may build a CONSTRUCT query instead of a SELECT, from easy to hard:
;;
;; - only `insert-patterns` or `delete-patterns` and [no GRAPH or not sudo]
;; - only `insert-patterns` or `delete-patterns` and not a sudo query
;; - `insert-patterns` and `delete-patterns` and [no GRAPH or not sudo]; with two queries
;;
;; The cases following this will require rewriting the CONSTRUCT part.  In essence: we only have three things
;; we can return in the CONSTRUCT.  But there are two aspects which we need to cater for:
;; 
;; 1. For sudo, there's G S P O to take into account, so one must be a known value.
;;
;; 2. In order to have one construction for insert-patterns and delete-patterns, we need one of S P O to be a known
;;    value for each quad statement.
;;
;; We can reasonably create a different CONSTRUCT if this is the case and construct the quads from there.
;;
;; - only `insert-patterns` or `delete-patterns` and sudo query with fixed single fixed GRAPH
;; - each `insert-pattern` or `delete-pattern` has one known resource (S P or O) and [no GARPH or not sudo]
;; - there is only a single pattern (all variables), and [known GRAPH or no GRAPH or not sudo]
;; - sudo with single pattern and G S P or O known

(defun filled-in-patterns (patterns bindings)
  "Creates a set of QUADS for the given patterns and bindings.

Any pattern which has no variables will be returned as is.  Any pattern
with bindings will be filled in for each discovered binding.  If any
variables are missing this will not lead to a pattern."
  (flet ((pattern-has-variables (pattern)
           (loop for (place match) on pattern by #'cddr
                 when (and (sparql-parser:match-p match)
                           (sparql-parser:match-term-p match 'ebnf::|VAR1| 'ebnf::|VAR2|))
                   do
                      (progn
                        ;; TODO: this should be integrated with *error-on-unwritten-data*.  If the quad doesn't exist
                        ;; _only_ because the graph is still a variable, then we would want to error on this case.
                        ;; (when (eq place :graph)
                        ;;   (format t "~&WARNING: Quad pattern contains graph variable ~A which is not supported, quad will be dropped ~A~%" match pattern))
                        (return t))))
         (fill-in-pattern (pattern bindings)
           (loop for (place match) on pattern by #'cddr
                 if (and (sparql-parser:match-p match)
                         (sparql-parser:match-term-p match 'ebnf::|VAR1| 'ebnf::|VAR2|)
                         (jsown:keyp bindings (subseq (terminal-match-string match) 1))) ; binding contains key (OPTIONAL in queries)
                   append (list place
                                (let ((solution (jsown:val bindings (subseq (terminal-match-string match) 1))))
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

(defun create-construct-query-for-modify (group-graph-pattern prefixes base quad-patterns)
  "Builds up a construct query to handle the MODIFY operation within constraints of what CONSTRUCT can achieve."
  (let ((construct-triples
          ;; subject, predicate :: '(VAR1 VAR2 IRIREF (CONS PNAME-LN iri-string) (CONS PNAME-NS iri-tring))
          ;; object :: '(VAR1 VAR2 ebnf::|BooleanLiteral| ebnf::|NumericLiteral| IRIREF (CONS PNAME-LN iri-string) (CONS PNAME-NS iri-tring) rdf-literal-with-expanded-datatype-if-exists)
          (loop for construct-triples = nil then construct-triples
                for quad-pattern in quad-patterns
                for subject = (getf quad-pattern :subject)
                for predicate = (getf quad-pattern :predicate)
                for object = (getf quad-pattern :object)
                do
                   (setf
                    construct-triples
                    (make-nested-match
                     `(ebnf::|ConstructTriples|
                             (ebnf::|TriplesSameSubject|
                                    (ebnf::|VarOrTerm|
                                           ,(if (consp subject)
                                                ;; it's an iri
                                                (make-nested-match `(ebnf::|GraphTerm| ,(make-iri (cdr subject))))
                                                ;; uri or variable
                                                (case (sparql-parser:match-term subject)
                                                  (ebnf::|VAR1| (make-nested-match `(ebnf::|Var| ,subject)))
                                                  (ebnf::|VAR2| (make-nested-match `(ebnf::|Var| ,subject)))
                                                  ;; TODO: check if IRIREF yields an IRIREF or a CONS or other
                                                  (ebnf::|IRIREF| (make-nested-match `(ebnf::|GraphTerm| (ebnf::|iri| ,subject)))))))
                                    (ebnf::|PropertyListNotEmpty|
                                           (ebnf::|Verb|
                                                  (ebnf::|VarOrIri|
                                                         ,(if (consp predicate)
                                                              ;; it's an iri
                                                              (make-iri (cdr predicate))
                                                              (case (sparql-parser:match-term predicate)
                                                                (ebnf::|VAR1| (make-nested-match `(ebnf::|Var| ,predicate)))
                                                                (ebnf::|VAR2| (make-nested-match `(ebnf::|Var| ,predicate)))
                                                                ;; TODO: check if IRIREF yields an IRIREF or a CONS or other
                                                                (ebnf::|IRIREF| (make-nested-match `(ebnf::|iri| ,predicate)))))))
                                           (ebnf::|ObjectList|
                                                  ;; ebnf::|BooleanLiteral| ebnf::|NumericLiteral| rdf-literal-with-expanded-datatype-if-exists
                                                  (ebnf::|Object|
                                                         (ebnf::|GraphNode|
                                                                (ebnf::|VarOrTerm|
                                                                       ,(if (consp object)
                                                                            ;; it's an iri
                                                                            (make-nested-match `(ebnf::|GraphTerm| ,(make-iri (cdr object))))
                                                                            ;; uri or variable
                                                                            (case (sparql-parser:match-term object)
                                                                              ((ebnf::|VAR1| ebnf::|VAR2|)
                                                                               (make-nested-match `(ebnf::|Var| ,object)))
                                                                              ;; TODO: check if IRIREF yields an IRIREF or a CONS or other
                                                                              (ebnf::|IRIREF|
                                                                               (make-nested-match `(ebnf::|GraphTerm| (ebnf::|iri| ,object))))
                                                                              ((ebnf::|BooleanLiteral| ebnf::|NumericLiteral| ebnf::|RDFLiteral|)
                                                                               (make-nested-match `(ebnf::|GraphTerm| ,object)))))))))))
                             "."
                             ,construct-triples)))
                finally (return construct-triples))))
    (when quad-patterns
     (assert (sparql-generator:is-valid (sparql-parser:make-sparql-ast :top-node construct-triples :string sparql-parser:*scanning-string*))
             (construct-triples)
             "CONSTRUCT-TRIPLES should be a valid AST.")
     (sparql-parser:make-sparql-ast
      :string sparql-parser:*scanning-string*
      :top-node (make-nested-match
                 `(ebnf::|QueryUnit|
                         (ebnf::|Query|
                                (ebnf::|Prologue|
                                       ,@(when base `(ebnf::|BaseDecl| "BASE" ,base))
                                       ,@(loop for (prefix . iriref) in prefixes
                                               collect
                                               `(ebnf::|PrefixDecl| "PREFIX" ,prefix ,iriref)))
                                (ebnf::|ConstructQuery|
                                       "CONSTRUCT"
                                       (ebnf::|ConstructTemplate|
                                              "{"
                                              ,construct-triples
                                              "}")
                                       (ebnf::|WhereClause|
                                              "WHERE"
                                              ,group-graph-pattern)
                                       (ebnf::|SolutionModifier|))
                                (ebnf::|ValuesClause|))))))))

(defun quads-for-construct-bindings (bindings)
  "Ceates a series of quads fo the constructed bindings."
  (loop for binding in bindings
        for subject = (jsown:val binding "s")
        for predicate = (jsown:val binding "p")
        for object = (jsown:val binding "o")
        ;; TODO: support difference in what Virtuoso yields for SELECT versus CONSTRUCT
        collect (list :subject (binding-as-match subject)
                      :predicate (binding-as-match predicate)
                      :object (binding-as-match object))))

(defun modify-operation-to-quads-through-construct (operation)
  "Uses a CONSTRUCT query to discover the quads which would apply for a MODIFY operation."
  (let* ((insert-patterns (operation-data-subfield operation :insert-patterns))
         (delete-patterns (operation-data-subfield operation :delete-patterns))
         (group-graph-pattern (operation-data-subfield operation :group-graph-pattern))
         (prefixes (operation-data-subfield operation :prefixes))
         (base (operation-data-subfield operation :base)))
    (assert (or (not insert-patterns) (not delete-patterns)) (insert-patterns delete-patterns)
            "CONSTRUCT based modify is only supported when INSERT-PATTERNS or DELETE-PATTERNS is nil.")
    (assert (not (connection-globals:mu-auth-sudo)) nil
            "CONSTRUCT based modify is only supported for non-construct queries")
    (let* ((construct-for-modify
             (create-construct-query-for-modify group-graph-pattern prefixes base (or insert-patterns delete-patterns)))
           (bindings
             (client:standardize-construct-bindings
              (client:batch-create-full-solution-for-construct-query
               construct-for-modify
               :for :modify :usage :read)))
           (quads (quads-for-construct-bindings bindings)))
      (if insert-patterns
          (list :insert-quads quads)
          (list :delete-quads quads)))))

(defun make-select-query-for-patterns (group-graph-pattern prefixes base &rest quad-pattern-groups)
  "Constructs a sparql-ast which can be executed as a query to extract patterns for quads."
  (let ((variables (delete-duplicates
                    (or
                     (loop for quad-patterns in quad-pattern-groups
                           append
                           (loop for quad-pattern in quad-patterns
                                 append
                                 (loop for (k v) on quad-pattern by #'cddr
                                       for v-match = (if (consp v) (car v) v)
                                       when (and v-match (sparql-parser:match-term-p v-match 'ebnf::|VAR1| 'ebnf::|VAR2|))
                                         collect v-match)))
                     (list (sparql-manipulation:make-word-match "*"))) ; match * if no variables found
                    :key #'terminal-match-string
                    :test #'string=))
        ;; We could parse group-graph-pattern through an adaptation of
        ;; the following code so we can verify the query is valid when
        ;; running.  We currently don't do this for performance reasons.
        ;; To execute this, write out the string as if it were valid,
        ;; then parse as GroupGraphPattern.

        ;; (group-graph-pattern-as-string (sparql-generator:write-valid (sparql-parser::make-sparql-ast
        ;;                                                               :top-node group-graph-pattern
        ;;                                                               :string sparql-parser::*scanning-string*)))
        )
    (sparql-parser:make-sparql-ast
     :string sparql-parser:*scanning-string*
     :top-node (make-nested-match
                `(ebnf::|QueryUnit|
                        (ebnf::|Query|
                               (ebnf::|Prologue|
                                      ,@(when base `(ebnf::|BaseDecl| "BASE" ,base))
                                      ,@(loop for (prefix . iriref) in prefixes
                                              collect
                                              `(ebnf::|PrefixDecl| "PREFIX" ,prefix ,iriref)))
                               (ebnf::|SelectQuery|
                                      (ebnf::|SelectClause|
                                             "SELECT"
                                             "DISTINCT"
                                             ,@(loop for var in variables collect `(ebnf::|Var| ,var)))
                                      (ebnf::|WhereClause|
                                             "WHERE"
                                             ,group-graph-pattern)
                                      (ebnf::|SolutionModifier|))
                               (ebnf::|ValuesClause|)))))))

(defun modify-operation-to-quads-through-select (operation)
  "Uses a SELECT query to discover the quads which would apply for a MODIFY operation."
  (let* ((insert-patterns (operation-data-subfield operation :insert-patterns))
         (delete-patterns (operation-data-subfield operation :delete-patterns))
         (group-graph-pattern (operation-data-subfield operation :group-graph-pattern))
         (prefixes (operation-data-subfield operation :prefixes))
         (base (operation-data-subfield operation :base))
         (query (make-select-query-for-patterns group-graph-pattern prefixes base insert-patterns delete-patterns))
         (bindings
           (client:batch-create-full-solution-for-select-query query :for :modify :usage :read)))
    (when bindings
      (let* ((filled-in-deletes (filled-in-patterns delete-patterns bindings))
             (filled-in-inserts (filled-in-patterns insert-patterns bindings)))
        ;; TODO: Optionally error when INSERT or DELETE template does not contain variables AND no solution in WHERE
        (list :delete-quads filled-in-deletes :insert-quads filled-in-inserts)))))

(defparameter *allow-construct-query-p* t
  "Can we use a CONSTRUCT query for detecting changes or not.")

(defun modify-operation-to-quads (operation)
  "For any modify operation, we will need to query the triplestore to know what needs to be done.  Some of these may be
handled through CONSTRUCT queries, some may only be handled through SELECT queries."
  (let ((insert-patterns (operation-data-subfield operation :insert-patterns))
        (delete-patterns (operation-data-subfield operation :delete-patterns)))
    (if (and *allow-construct-query-p*
             (not (connection-globals:mu-auth-sudo))
             (or (null insert-patterns)
                 (null delete-patterns)))
        ;; handle through CONSTRUCT
        (modify-operation-to-quads-through-construct operation)
        ;; handle through SELECT and filling in patterns
        (modify-operation-to-quads-through-select operation))))

(defun quad-operation-to-quads (operation)
  "Converts a quad-operation to the corresponding insert-quads and delete-quads."
  ;; TODO: handle WITH iriref which should be removed for non sudo queries
  (case (operation-type operation)
    (:insert-triples
     (list :insert-quads (operation-data operation)))
    (:delete-triples
     (list :delete-quads (operation-data operation)))
    (:modify
     (modify-operation-to-quads operation))))
