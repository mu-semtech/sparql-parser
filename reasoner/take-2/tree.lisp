(in-package :reasoner-variables)

(handle ebnf::|QueryUnit|
        :process (ebnf::|Query|))

(handle ebnf::|Query|
        :process (ebnf::|Prologue|))

(handle ebnf::|Prologue|
        ;; processing of other handler
        )

(handle ebnf::|SelectQuery|
        :accept (ebnf::|SelectClause|))

(handle ebnf::|WhereClause|
        :process (ebnf::|GroupGraphPattern|))

(handle ebnf::|GroupGraphPattern|
        :process (ebnf::|GroupGraphPatternSub|))

(handle ebnf::|TriplesBlock|
        :local-context (:subject nil
                        :predicate nil
                        :object nil)
        :process (ebnf::|TriplesBlock|
                  ebnf::|TriplesSameSubjectPath|))

(handle ebnf::|TriplesSameSubjectPath|
        :local-context (:triple-processing 'subject)
        :process (ebnf::|VarOrTerm|))

(handle ebnf::|PropertyListPathNotEmpty|
        :local-context (:triple-processing 'predicate)
        :process ebnf::|VerbPath|)

(handle ebnf::|Path|
        :process ebnf::|Pathalternative|)

(handle ebnf::|PathAlternative|
        :process ebnf::|PathSequence|)

(handle ebnf::|PathSequence|
        :process ebnf::|PathEltOrInverse|)

(handle ebnf::|PathEltOrInverse|
        :process ebnf::|PathElt|)

(handle ebnf::|PathElt|
        :process ebnf::|PathPrimary|)

(handle ebnf::|PathPrimary|
        :process-functions ((ebnf::|Iri| (iri)
                                   (setf (info-predicate *info*) iri)
                                   (has-iri-relation (info-subject *info*)
                                                     iri
                                                     _))))

(handle ebnf::|Objectlistpath|
        :process ebnf::|Objectpath|)

(handle ebnf::|Objectpath|
        :process ebnf::|Graphnodepath|)

(handle ebnf::|Graphnodepath|
        :process ebnf::|Varorterm|)

(handle ebnf::|Varorterm|
        :process ebnf::|Graphterm|)

(handle ebnf::|Graphterm|
        :process-functions ((ebnf::|Iri| (iri)
                                   (has-iri-relation (info-subject *info*)
                                                     (info-predicate *info*)
                                                     iri))))



(handle ebnf::|VarOrTerm|
        :process-functions ((ebnf::|Var| (var)
                                   (collect-var var)
                                   (setf (info-subject *info*) var)))
        :process (ebnf::|GraphTerm|))

(handle ebnf::|GraphTerm|
        :process-functions ((ebnf::|Iri| (iri)
                                   (setf (info-subject *info*) iri))))

