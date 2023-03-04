(in-package #:detect-quads)

;;;;;;;;;;;;;;;;;
;;;; Detect quads
;;;;
;;;; Processes an EBNF under the assumption that it is correctly
;;;; constructed.  Yields the quads that can be extracted from the
;;;; query.

(handle ebnf::|UpdateUnit|
        :process (ebnf::|Update|))
(handle ebnf::|Update|
        :note "An update has a (possibly empty) prologue.  This data structure needs to be extended for each update passed down."
        :note "Cycling back to update means we should only determine the next query after this set of quads was fully processed.  In the future this will create its own set of challenges in terms of locking because we can't know what is to be stored where."
        :todo "ebnf::|Update| should understand nested updates (split with a ';' semicolon) and provide an execution path for them."
        :not-supported (ebnf::|Update|)
        :process (ebnf::|Prologue| ebnf::|Update1|)
        :local-context (:prefixes nil ;; I don't think we should reset this when hitting update again but rather should extend it.
                        :base nil))
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
        :process (ebnf::|InsertData| ebnf::|DeleteData| ebnf::|DeleteWhere| ebnf::|Modify| ebnf::|Add|)
        :not-supported (ebnf::|Load| ebnf::|Clear| ebnf::|Drop| ebnf::|Move| ebnf::|Copy| ebnf::|Create|))
(handle ebnf::|InsertData|
        :local-context (:quads nil)
        :todo "Collect all quads and return them as quads-to-insert"
        :process (ebnf::|QuadData|)
        :after ((response match)
                (declare (ignore response match))
                `(:operation (:insert-triples ,(info-quads *info*)))))
(handle ebnf::|QuadData|
        :process (ebnf::|Quads|))
(handle ebnf::|Quads|
        :process (ebnf::|TriplesTemplate| ebnf::|QuadsNotTriples|))
(handle ebnf::|QuadsNotTriples|
        :local-context (:graph nil)
        :process-functions ((ebnf::|VarOrIri| (var-or-iri)
                                   (setf (info-graph *info*)
                                         (detect-quads-processing-handlers::|VarOrIri| var-or-iri))))
        :process (ebnf::|TriplesTemplate|))
(handle ebnf::|TriplesTemplate|
        :process (ebnf::|TriplesSameSubject| ebnf::|TriplesTemplate|))
(handle ebnf::|TriplesSameSubject|
        :local-context (:subject nil)
        :process-functions ((ebnf::|VarOrTerm| (var-or-term)
                                   (setf (info-subject *info*)
                                         (detect-quads-processing-handlers::|VarOrTerm| var-or-term))))
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
                                   (push `(:graph ,(info-graph *info*)
                                           :subject ,(info-subject *info*)
                                           :predicate ,(info-predicate *info*)
                                           :object ,(detect-quads-processing-handlers::|Object| object))
                                         (info-quads *info*)))))
(handle ebnf::|Verb|
        :note "Processed manually because Verb may be 'a', which needs to be treated specially."
        :todo "Create constant for rdf:type which could be understood in many places for reference of \"a\"."
        :function ((verb)
                   (let ((submatch (first (sparql-parser:match-submatches verb))))
                     (if (stringp (match-term submatch))
                         (sparql-manipulation:iriref "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                         (detect-quads-processing-handlers::|VarOrIri| submatch)))))
(handle ebnf::|VarOrIri|
        :process (ebnf::|Var| ebnf::|iri|))
(handle ebnf::|Object|
        :process (ebnf::|GraphNode|))
(handle ebnf::|GraphNode|
        :process (ebnf::|VarOrTerm|)
        :not-supported (ebnf::|TriplesNode|))

(handle ebnf::|VarOrTerm|
        :process (ebnf::|Var| ebnf::|GraphTerm|))
(handle ebnf::|GraphTerm|
        :todo "Further expand boolean literal."
        :todo "Document RDFLiteral not being processed further."
        :process (ebnf::|iri| ebnf::|NumericLiteral|)
        :accept (ebnf::|RDFLiteral| ebnf::|BooleanLiteral|)
        :not-supported (ebnf::|BlankNode| ebnf::|NIL|))
(handle ebnf::|iri|
        :process (ebnf::|PrefixedName|)
        :accept (ebnf::|IRIREF|))
(handle ebnf::|PrefixedName|
        ;; TODO implement extraction of information from prefixed name based on prefixes
        )
(handle ebnf::|NumericLiteral|
        :process (ebnf::|NumericLiteralUnsigned| ebnf::|NumericLiteralPositive| ebnf::|NumericLiteralNegative|))
(handle ebnf::|NumericLiteralUnsigned|
        :accept (ebnf::|INTEGER| ebnf::|DECIMAL| ebnf::|DOUBLE|))
(handle ebnf::|NumericLiteralPositive|
        :accept (ebnf::|INTEGER_POSITIVE| ebnf::|DECIMAL_POSITIVE| ebnf::|DOUBLE_POSITIVE|))
(handle ebnf::|NumericLiteralNegative|
        :accept (ebnf::|INTEGER_NEGATIVE| ebnf::|DECIMAL_NEGATIVE| ebnf::|DOUBLE_NEGATIVE|))
(handle ebnf::|Var|
        :accept (ebnf::|VAR1| ebnf::|VAR2|))
