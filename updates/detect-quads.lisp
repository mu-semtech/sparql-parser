(in-package #:detect-quads)

;;;;;;;;;;;;;;;;;
;;;; Detect quads
;;;;
;;;; Processes an EBNF under the assumption that it is correctly
;;;; constructed.  Yields the quads that can be extracted from the
;;;; query.
;;;;
;;;; The detection of operations can yield quads.  These can contain the
;;;; following elements: ebnf::|VAR1| , ebnf::|VAR2| , ebnf::|IRIREF| ,
;;;; (CONS ebnf::|PNAME_LN| URI-STRING) , (CONS ebnf::|PNAME_NS| URI-STRING) ,
;;;; ebnf::|RDFLiteral| , ebnf::|BooleanLiteral| , ebnf::|NumericLiteral|
;;;;
;;;; Note the enrichment of PNAME_LN and PNAME_NS in which their string
;;;; representation is overwritten by the full URI representation.

(handle ebnf::|UpdateUnit|
        :local-context (;; TODO: Prefixes and Base should not stick
                        ;; through Update portions but this is assumed
                        ;; in our stack.  Based on
                        ;; [[https://www.w3.org/TR/2013/REC-sparql11-update-20130321/#deleteData]]
                        ;; Example 4 and seen in
                        ;; [[https://www.w3.org/TR/2013/REC-sparql11-update-20130321/##mappingRequestsToOperations]]
                        ;; R_1 ; R2 = Tr(Tr(GS, R_1), Tr(GS, R_2)) in
                        ;; which a new GS is produced by R_1 on which
                        ;; R_2 is applied.
                        ;; TODO: Process WITH GRAPH for IRIREF which should alter query and default graph
                        :operations nil
                        :prefixes (list
                                   (cons (sparql-parser:make-match
                                          :term 'ebnf::|PNAME_NS|
                                          :submatches (list (sparql-parser:make-scanned-token
                                                             :start 0 :end 0 :token 'ebnf::|PNAME_NS|
                                                             :string "xsd:")))
                                         (sparql-manipulation:iriref "http://www.w3.org/2001/XMLSchema#")))
                        :base nil)
        :process (ebnf::|Update|)
        :after ((response match)
                (declare (ignore response match))
                ;; (format t "~&PREFIXES are ~A~%" (info-prefixes *info*))
                (info-operations *info*)))
(handle ebnf::|Update|
        :note "An update has a (possibly empty) prologue.  This data structure needs to be extended for each update passed down."
        :note "Cycling back to update means we should only determine the next query after this set of quads was fully processed.  In the future this will create its own set of challenges in terms of locking because we can't know what is to be stored where."
        :todo "ebnf::|Update| should understand nested updates (split with a ';' semicolon) and provide an execution path for them."
        ;; :not-supported (ebnf::|Update|)
        :process (ebnf::|Prologue| ebnf::|Update1| ebnf::|Update|))
(handle ebnf::|Prologue|
        :process (ebnf::|BaseDecl| ebnf::|PrefixDecl|))
(handle ebnf::|BaseDecl|
        :process-functions ((ebnf::|IRIREF| (iriref)
                                   (setf (info-base *info*) iriref))))
(handle ebnf::|PrefixDecl|
        :local-context (:pname-ns nil)
        :process-functions ((ebnf::|PNAME_NS| (pname-ns)
                                  (setf (info-pname-ns *info*) pname-ns))
                            (ebnf::|IRIREF| (iriref)
                                  (push (cons (info-pname-ns *info*)
                                              iriref)
                                        (info-prefixes *info*)))))

(handle ebnf::|Update1|
        :process (ebnf::|InsertData| ebnf::|DeleteData| ebnf::|DeleteWhere| ebnf::|Modify|)
        :not-supported (ebnf::|Load| ebnf::|Clear| ebnf::|Drop| ebnf::|Move| ebnf::|Copy| ebnf::|Create| ebnf::|Add|))
(handle ebnf::|InsertData|
        :local-context (:quads nil)
        :process (ebnf::|QuadData|)
        :after ((response match)
                (declare (ignore response match))
                (alexandria:appendf
                 (info-operations *info*)
                 `((:insert-triples ,@(info-quads *info*))))))
(handle ebnf::|DeleteWhere|
        :local-context (:quads nil)
        :process-functions ((ebnf::|QuadPattern| (quad-pattern)
                                   (detect-quads-processing-handlers::|QuadPattern| quad-pattern)
                                   (alexandria:appendf
                                    (info-operations *info*)
                                    `((:modify (:delete-patterns ,(info-quads *info*)
                                                :query ,(make-select-query-for-patterns
                                                         quad-pattern
                                                         (reverse (info-prefixes *info*))
                                                         (info-base *info*)
                                                         (info-quads *info*)))))))))
(handle ebnf::|DeleteData|
        :local-context (:quads nil)
        :process (ebnf::|QuadData|)
        :after ((response match)
                (declare (ignore response match))
                (alexandria:appendf
                 (info-operations *info*)
                 `((:delete-triples ,@(info-quads *info*))))))
(handle ebnf::|QuadData|
        :process (ebnf::|Quads|))
(handle ebnf::|Quads|
        :process (ebnf::|TriplesTemplate| ebnf::|QuadsNotTriples|))
(handle ebnf::|QuadsNotTriples|
        :local-context (:graph nil)
        :process-functions ((ebnf::|VarOrIri| (var-or-iri)
                                   (setf (info-graph *info*)
                                         (expand-var-or-term
                                          (detect-quads-processing-handlers::|VarOrIri| var-or-iri)))))
        :process (ebnf::|TriplesTemplate|))
(handle ebnf::|TriplesTemplate|
        :process (ebnf::|TriplesSameSubject| ebnf::|TriplesTemplate|))
(handle ebnf::|TriplesSameSubject|
        :local-context (:subject nil)
        :process-functions ((ebnf::|VarOrTerm| (var-or-term)
                                   (setf (info-subject *info*)
                                         (expand-var-or-term
                                          (detect-quads-processing-handlers::|VarOrTerm| var-or-term)))))
        :process (ebnf::|PropertyListNotEmpty|)
        :not-supported (ebnf::|TriplesNode| ebnf::|PropertyList|))
(handle ebnf::|PropertyListNotEmpty|
        :local-context (:predicate nil)
        :todo "Handle Verb"
        :process-functions ((ebnf::|Verb| (verb)
                                   (setf (info-predicate *info*)
                                         (detect-quads-processing-handlers::|Verb| verb))))
        :process (ebnf::|ObjectList|))
(handle ebnf::|ObjectList|
        :process-functions ((ebnf::|Object| (object)
                                   (push (quad:make-quad :graph (info-graph *info*)
                                                         :subject (info-subject *info*)
                                                         :predicate (info-predicate *info*)
                                                         :object (detect-quads-processing-handlers::|Object| object))
                                         (info-quads *info*)))))
(handle ebnf::|Verb|
        :note "Processed manually because Verb may be 'a', which needs to be treated specially."
        :todo "Create constant for rdf:type which could be understood in many places for reference of \"a\"."
        :function ((verb)
                   (let ((submatch (first (sparql-parser:match-submatches verb))))
                     (if (stringp (match-term submatch))
                         (sparql-manipulation:iriref "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                         (expand-var-or-term
                          (detect-quads-processing-handlers::|VarOrIri| submatch))))))
(handle ebnf::|VarOrIri|
        :process (ebnf::|Var| ebnf::|iri|))
(handle ebnf::|Object|
        :process (ebnf::|GraphNode|))
(handle ebnf::|GraphNode|
        :process (ebnf::|VarOrTerm|)
        :not-supported (ebnf::|TriplesNode|))

(handle ebnf::|VarOrTerm|
        :process (ebnf::|Var| ebnf::|GraphTerm|))

(defun expand-var-or-term (thing)
  "Converts THING into an IRIREF if it is a URL or was parsed as a full URL
through detect-quads-processing-handlers::|VarOrIri| or
detect-quads-processing-handlers::|VarOrTerm|."
  (cond ((consp thing)
         (sparql-manipulation:iriref (cdr thing)))
        (t thing)))

(handle ebnf::|GraphTerm|
        :todo "Further expand boolean literal."
        :process (ebnf::|iri| ebnf::|RDFLiteral|)
        :accept (ebnf::|BooleanLiteral| ebnf::|NumericLiteral|)
        :not-supported (ebnf::|BlankNode| ebnf::|SPARQLNIL|))
(handle ebnf::|RDFLiteral|
        :function ((rdf-literal)
                   (let ((submatches (sparql-parser:match-submatches rdf-literal)))
                     (if (= (length submatches) 3)           ; must be with iri definition
                         (let* ((iri (third submatches))
                                (expanded-iri
                                  (sparql-parser:make-match
                                   :term 'ebnf::|iri|
                                   :submatches (list (expand-var-or-term (detect-quads-processing-handlers::|iri| iri))))))
                           (setf (third (sparql-parser:match-submatches rdf-literal))
                                 expanded-iri)
                           rdf-literal)
                         rdf-literal))))

(handle ebnf::|iri|
        :todo "This is not consistent.  IRIREF probably emits other content than PrefixedName."
        :process (ebnf::|PrefixedName|)
        :accept (ebnf::|IRIREF|))
(handle ebnf::|PrefixedName|
        :process (ebnf::|PNAME_LN| ebnf::|PNAME_NS|))
(handle ebnf::|PNAME_LN|
        :function ((pname-ln)
                   (let* ((string (primitive-match-string pname-ln))
                          (split-idx (search ":" string))
                          (pname-ns (subseq string 0 (1+ split-idx)))
                          (pn-local (subseq string (1+ split-idx)))
                          (found-prefix (find pname-ns (info-prefixes *info*)
                                              :key (alexandria:compose #'primitive-match-string #'car)
                                              :test #'string=)))
                     (assert found-prefix)
                     (let ((prefix-uri-representation (primitive-match-string (cdr found-prefix))))
                       ;; (format t "~&Found prefix uri representation ~A with pn-local ~A~%" prefix-uri-representation pn-local)
                       (cons pname-ln
                             (concatenate 'string
                                          (sparql-manipulation:uri-unwrap-marks prefix-uri-representation)
                                          pn-local))))))
(handle ebnf::|PNAME_NS|
        :function ((pname-ns)
                   (let* ((string (primitive-match-string pname-ns))
                          (found-prefix (find string (info-prefixes *info*)
                                              :key (alexandria:compose #'primitive-match-string #'car)
                                              :test #'string=)))
                     (assert found-prefix)
                     (let ((pname-ns-uri-representation (primitive-match-string (cdr found-prefix))))
                       (cons pname-ns
                             (concatenate 'string
                                          (sparql-manipulation:uri-unwrap-marks pname-ns-uri-representation)
                                          string))))))
(handle ebnf::|NumericLiteral|
        :accept (ebnf::|NumericLiteralUnsigned| ebnf::|NumericLiteralPositive| ebnf::|NumericLiteralNegative|))
(handle ebnf::|Var|
        :accept (ebnf::|VAR1| ebnf::|VAR2|))

(handle ebnf::|Modify|
        :todo "make WITH iri not leak to next query separated by ;"
        :local-context (:delete-quad-patterns nil
                        :insert-quad-patterns nil
                        :quads nil)
        :process-functions
        ((ebnf::|iri| (iri)
                ;; Must belong to the WITH clause
                (setf (info-graph *info*)
                      (expand-var-or-term
                       (detect-quads-processing-handlers::|iri| iri))))
         (ebnf::|DeleteClause| (delete-clause)
                (detect-quads-processing-handlers::|DeleteClause| delete-clause)
                (setf (info-delete-quad-patterns *info*)
                      (info-quads *info*))
                (setf (info-quads *info*) nil))
         (ebnf::|InsertClause| (insert-clause)
                (detect-quads-processing-handlers::|InsertClause| insert-clause)
                (setf (info-insert-quad-patterns *info*)
                      (info-quads *info*))
                (setf (info-quads *info*) nil))
         (ebnf::|GroupGraphPattern| (group-graph-pattern)
                (let ((delete-patterns (info-delete-quad-patterns *info*))
                      (insert-patterns (info-insert-quad-patterns *info*))
                      (expanded-group-graph-pattern
                        (if (info-graph *info*)
                            (handle-update-unit::make-nested-match
                             `(ebnf::|GroupGraphPattern|
                                     "{"
                                     (ebnf::|GroupGraphPatternSub|
                                            (ebnf::|GraphPatternNotTriples|
                                                   (ebnf::|GraphGraphPattern|
                                                          "GRAPH"
                                                          (ebnf::|VarOrIri|
                                                                 (ebnf::|iri|
                                                                        ,(info-graph *info*)))
                                                          ,group-graph-pattern)))
                                     "}"))
                            group-graph-pattern)))
                  (let ((modify `((:modify (:delete-patterns ,delete-patterns
                                            :insert-patterns ,insert-patterns
                                            :query ,(make-select-query-for-patterns expanded-group-graph-pattern
                                                                                    (reverse (info-prefixes *info*))
                                                                                    (info-base *info*)
                                                                                    insert-patterns
                                                                                    delete-patterns))))))
                    (alexandria:appendf (info-operations *info*) modify)))))
        :not-supported (ebnf::|UsingClause|))
(handle ebnf::|DeleteClause|
        :process (ebnf::|QuadPattern|))
(handle ebnf::|InsertClause|
        :process (ebnf::|QuadPattern|))
(handle ebnf::|QuadPattern|
        :process (ebnf::|Quads|))

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
                    :key #'primitive-match-string
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
     :top-node (handle-update-unit::make-nested-match
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

(defun primitive-match-string (match)
  "We consider a primitive match to be a match which has a
sparql-parser:scanned-token as its only child element.  This returns its
string representation."
  (assert (and (typep (first (sparql-parser:match-submatches match)) 'sparql-parser:scanned-token)
               (= (length (sparql-parser:match-submatches match)) 1)))
  (sparql-parser:scanned-token-effective-string (first (sparql-parser:match-submatches match))))

(defun quad-term-uri (quad-term)
  "Yields the quad-term's URI representation if that exists."
  (cond ((consp quad-term)
         (cdr quad-term))
        ((sparql-parser:match-term-p quad-term 'ebnf::|IRIREF|)
         (let ((str (primitive-match-string quad-term)))
           (subseq str 1 (1- (length str)))))))

(defun quad-term-uri= (quad-term uri-string)
  "Checks whether uri-string is the same as the quad-term."
  ;; The quad-uri-string is wrapped in < and > but the uri-string is
  ;; not, so we need to unwrap.
  (let* ((quad-uri-string (quad-term-uri quad-term)))
    (if quad-uri-string
        (string= uri-string quad-uri-string))))

;;;;;;;;;;;
;;; helpers
;;;

(defun operation-type (operation)
  "Yields the type of the operation, one of :insert-triples :delete-triples or :modify."
  (car operation))

(defun operation-data (operation)
  "Yields the data belonging to the operation."
  (cdr operation))

(defun operation-data-subfield (operation subfield)
  "Yields the subfield of an operation.
This only exists for :modify and it supports :delete-patterns
:insert-patterns :query.  If one could not be found an empty list is
returned."
  (getf (car (operation-data operation)) subfield))

