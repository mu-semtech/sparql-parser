(in-package #:reasoner)

;; Much can be derived from a basic knowledge base.  This component aims
;; to derive as much as possible from the query itself, so the
;; information can later be used to make the query faster, and thus
;; easier to execute and better to cache.

;; For example:
;; 0 | A  | B          | C
;; 1 | ?s   a            foaf:Person.
;; 2 | ?s   ?foaf:name   ?name.
;; 3 | ?s   ?foaf:mbox   <mailto:aad.versteden@redpencil.io>

;; Domain model

;; (:inheritance "foaf:Agent" "foaf:Person")
;; (:property "foaf:name" :source-type "foaf:Person")
;; (:property "foaf:mbox" :source-type "foaf:Agent" :target-type "foaf:Mbox")

;; Much can be derived from this:

;; From rule 3B we could derive:
;; 3 -> 3A type is (sub)type of "foaf:Agent"
;;   -> 3C is of type "foaf:Mbox"
;;   -> 2A type is (sub)type of "foaf:Agent"
;;   -> 1A type is (sub)type of "foaf:Agent"
;;   -> 3B subject has (sub)type of "foaf:Agent"
;;   -> 2B subject has (sub)type of "foaf:Agent"
;;   -> 1B subject has (sub)type of "foaf:Agent"
;;   -> 3B object has (sub)type of "foaf:Mbox"

;; It is obvious some derivations can be made in such a SPARQL query.
;; When we know one of the parts of the query, said knowledge
;; translates to other parts of the query.
;;
;; 1A -type-> 2A, 3A
;; 2A -type-> 1A, 3A
;; 3A -type-> 1A, 2A

;; This also translates into subqueries but there it translates into a
;; unidirectional set of statements.

;; 0 | A  | B          |
;; 1 | ?s   ?foaf:mbox   <mailto:aad.versteden@redpencil.io>.
;; 2 | FILTER NOT EXISTS {
;; 3 |  ?s  ?foaf:name   ?name.
;; 4 | }

;; We can derive
;; 1 -> B1 subject has (sub)type of "foaf:Agent"
;;   -> ?s in 1 has (sub)type of "foaf:Agent"
;;   -> ?s in 3 has (sub)type of "foaf:Agent"
;; 3 -> ?s in 3 has type of "foaf:Person"
;;   X> cannot derive ?s in 1 has type of "foaf:Person" because filter
;;      does not bind outside.

(defun derived-knowledge (query)
  ;; Extracts directly known knowledge from the query.  This may derive
  ;; information from each triple.
  (let* ((prefixes (extract-prefixes query))
         (expanded-uris (derive-expanded-uris query prefixes))
         (extracted-info (extract-constraints query expanded-uris prefixes))
         (derivations (extract-derivation-tree query)))
    (values ;; (derive-knowledge extracted-info derivations domain-model expanded-uris)
            prefixes
            expanded-uris
            extracted-info
            derivations)))

;; These two parameters indicate what we know of the left and the right
;; parts of the path.
;;
;; Depending on the path itself (which may be an inverse path) the
;; knowledge we have may need to be distributed differently.

(defparameter *node-knowledge* (make-hash-table :test 'eq)
  "What we know of the left part of the path.")

(defun node-knowledge (match key)
  "Yields the knoledge we have on KEY for MATCH."
  (getf (gethash match *node-knowledge*) key))

(defun (setf node-knowledge) (value match key)
  (setf (getf (gethash match *node-knowledge*) key) value))

(defmacro with-local-node-knowledge (&body body)
  "Execute BODY with a local node knowledge store."
  `(let ((*node-knowledge* (make-hash-table :test 'eq)))
     ,@body))

(defun match-has-direct-term (match term)
  "Yields truethy iff MATCH's children contain submatch named TERM."
  (typecase term
    (string (loop for m in (sparql-parser:match-submatches match)
                  when (string= term (sparql-parser:match-term m))
                    return m))
    (symbol (loop for m in (sparql-parser:match-submatches match)
                  when (eq term (sparql-parser:match-term m))
                    return m))))

(defmacro term-case (match &body cases)
  "CASE on the MATCH-TERM of MATCH."
  `(case (sparql-parser:match-term ,match)
     ,@cases))

(defun extract-constraints (query match-uri-mapping prefixes)
  "Extract direct information from each part of the matches of QUERY, using
PREFIXES for the expanded URIs."
  ;; Figures out which constraints are defined in the query.

  ;; We intend to assign the knowledge to the upper-most EBNF node on
  ;; which we can define the knowledge.  This rule may allow us to make
  ;; a clear decision on where to position the knowledge.

  ;; Eg: a triple ?s a foaf:Person would derive indicate ?s must have an
  ;; rdf:type relationship to foaf:Person.  The expansions are used to
  ;; further extract information from these items.
  (labels ((process-subject (same-subject-match)
             ;; same-subject-match is an ebnf::|TriplesSameSubject| or
             ;; ebnf::|TriplesSameSubjectPath|
             (when (match-has-direct-term same-subject-match 'ebnf::|VarOrTerm|)
               (sparql-manipulation::scan-deep-term-case sub (same-subject-match ebnf::|TriplesSameSubjectPath|)
                 ;; TODO: this will fail on RDFLiteral
                 (ebnf::|ABSTRACT-IRI| (process-subject-and-uri same-subject-match sub))
                 (ebnf::|ABSTRACT-VAR| (process-subject-and-var same-subject-match sub)))))
           (process-subject-and-uri (same-subject-match term)
             ;; same-subject-match :: TriplesSameSubjectPath or TriplesSameSubjectPath
             ;; term :: Expandable URI for subject

             ;; DONE: Enrich predicate match
             ;; TODO: Drill down to predicate (and enrich uri with knowledge based on predicate)
             (with-named-child (property-list-match)
                 (same-subject-match ebnf::|PropertyListPathNotEmpty|)
               (process-object :same-subject-match same-subject-match :subject-uri term)))
           (process-subject-and-var (same-subject-match term)
             ;; same-subject-match :: TriplesSameSubjectPath or TriplesSameSubject
             ;; term :: VAR for subject

             ;; TODO: Drill down to predicate (and enrich uri with knowledge based on predicate)
             ;; (format t "Working through ~A ~A" same-subject-match term)
             (with-named-child (property-list-match)
                 (same-subject-match ebnf::|PropertyListPathNotEmpty|)
               (process-object :same-subject-match same-subject-match :subject-var (first (sparql-parser:match-submatches term)))))
           ;; (extract-match-uri-mapping (match)
           ;;   (cached-expanded-uri match
           ;;                        :prefixes prefixes
           ;;                        :match-uri-mapping match-uri-mapping))
           ;; (extract-match-string (match)
           ;;   (sparql-parser:scanned-token-effective-string (first (sparql-parser:match-submatches match))))
           (process-object (&key same-subject-match subject-uri subject-var)
             ;; same-subject-match :: TriplesSameSubjectPath or TriplesSameSubject
             ;; subject-uri :: ABSTRACT-URI for subject or nil
             ;; subject-var :: ABSTRACT-VAR for subject or nil

             ;; (format nil "process-object got called with ~A ~A ~A" same-subject-match subject-uri subject-var)
             (with-named-child (child) (same-subject-match ebnf::|PropertyListPathNotEmpty|)
               (do-grouped-children (predicate-path objects-match)
                   (child :amount 2
                          :filter-terms (ebnf::|VerbPath| ebnf::|VerbSimple| ebnf::|ObjectList| ebnf::|ObjectListPath|)
                          :error-on-incomplete-amount-p t)
                 ;; ObjectList or ObjectListPath
                 ;; these objects will contain extra information
                 (do-grouped-children (single-object-match) (objects-match :filter-terms (ebnf::|ObjectPath| ebnf::|Object|))
                   ;; TODO: Correctly support or break in case of RDFLiteral
                   (let ((properties (list same-subject-match :predicate predicate-path)))
                     (when subject-var
                       (alexandria:appendf properties (list :left-var subject-var)))
                     (when subject-uri
                       (alexandria:appendf properties (list :left-uri subject-uri)))
                     (sparql-manipulation::scan-deep-term-case sub (single-object-match ebnf::|TriplesSameSubjectPath|)
                       (ebnf::|ABSTRACT-IRI| (alexandria:appendf properties (list :right-uri sub)))
                       (ebnf::|ABSTRACT-VAR| (alexandria:appendf properties (list :right-var sub)))
                       (ebnf::|ABSTRACT-PRIMITIVE| (alexandria:appendf properties (list :right-primitive sub))))
                     (apply (function extract-info) properties))))))
           (extract-info (match &key left-var left-uri predicate right-var right-uri right-primitive)
             (when left-var (setf (node-knowledge predicate :left-var) left-var))
             (when left-uri (setf (node-knowledge predicate :left-uri) left-uri))
             (when right-var (setf (node-knowledge predicate :right-var) right-var))
             (when right-uri (setf (node-knowledge predicate :right-uri) right-uri))
             (when right-primitive (setf (node-knowledge predicate :right-primitive) right-primitive))
             ;; TODO: support predicate as a real path instead
             (with-known-local-prefixes (:prefixes prefixes :uri-mapping match-uri-mapping)
               (reasoner-term-info:add-subject-predicate-object match
                                                                (or left-var left-uri)
                                                                predicate
                                                                (or right-var right-uri right-primitive)))))
    (loop-matches-symbol-case (match) query
      ;; Interpret subject
      ;; Drill down for predicate and object
      (ebnf::|TriplesSameSubjectPath| (process-subject match))
      (ebnf::|TriplesSameSubject| (process-subject match)))))

;; These properties can be used to push information through various levels
(defparameter handler-pass-had-change nil
  "Special case indicating whether the pass yielded a change of information or not.")

(defun match-match-submatches (match)
  "Yields all submatches of match which are a match themselves as per MATCH-P"
  (when (sparql-parser:match-p match)
    (remove-if-not #'sparql-parser:match-p (sparql-parser:match-submatches match))))

(defun handle-peers-pass (match)
  "Passes upstream knowledge to peers, and from peers to peers."
  (let* ((submatches (match-match-submatches match))
         (known-info (apply #'union-term-info (cons match submatches))))
    (dolist (submatch submatches)
      (setf (term-info submatch)
            known-info))))

(defun handle-down-pass (match)
  "Passes knowledge from the current match to its children."
  (dolist (submatch (match-match-submatches match))
    (setf (term-info submatch) (union-term-info match submatch))))

(defun handle-up-pass (match)
  "Passes knowledge from the match's children to the current match."
  (match-term match)
  (setf (term-info match)
        (apply #'union-term-info (cons match (match-match-submatches match)))))

(defun handle-up-options-pass (match)
  "Passes knowledge from the match's children to the parent as a series of options."
  ;; find all option clauses from children
  ;; combine them x with parent
  (setf (term-info match)
        (apply #'union-term-info match (match-match-submatches match))))

(defun handle-up-negative-pass (match)
  "Constructs a negative pass, indicating the following is something that would *not* resolve."
  (declare (ignore match))
  ;; TODO: construct negative mach for MINUS
  nil)

(defgeneric handle-custom-term (term match)
  (:documentation "Handles a custom approach for the given match.")
  (:method ((term (eql 'ebnf::|TriplesBlock|)) (match sparql-parser:match))
    ;; Unless calculated before, we should extract all available information.
    (format t "~&Handling custom term ~A for match ~&~A~&" term match))
  (:method (term (match sparql-parser:match))
    ;; Unless calculated before, we should extract all available information.
    (format t "~&No term custom matcher found for ~A for SPARQL-PARSER:MATCH~%" term)))

(defun handle-custom (match)
  (handle-custom-term (sparql-parser:match-term match) match))

(defun walk-distribute-match-1 (match)
  "Runs all distribution efforts for MATCH once."
  (when (match-p match)
    (dolist (processor (getf *information-distribution-approaches*
                             (match-term match)
                             (list #'handle-down-pass)))
      (funcall processor match))
    (dolist (submatch (match-submatches match))
      (walk-distribute-match-1 submatch))))

(defparameter *information-distribution-approaches* nil
  "All approaches for distributing information between nodes.")

(defmacro define-handler (pattern &rest approaches)
  `(alexandria:appendf
    *information-distribution-approaches*
    (list ',(intern (symbol-name pattern) (find-package :ebnf))
          (list ,@(mapcar (lambda (term)
                            `(function ,(intern (concatenate 'string "HANDLE-" (symbol-name term)))))
                          approaches)))))

;; Options
;;
;; down-pass :: any exposed information from our parent applies to our children but no information from the children applies to the parents (default)
;; up-pass :: any exposed infornmation from our children applies to our parent
;; peers-pass :: any exposed information on any of the children, applies to the other children
;; up-options-pass :: each of our children represents a set of optional constraints which should be passed up
;; up-negative-pass :: passes the constraints of our children (unioned) as not allowed in our parent (TODO)

(define-handler |SelectQuery| down-pass peers-pass)
(define-handler |WhereClause| up-pass down-pass)
(define-handler |GroupGraphPattern| up-pass down-pass)
(define-handler |SubSelect| down-pass peers-pass) ;; TODO: expand SubSelect
(define-handler |GroupGraphPatternSub| peers-pass)
(define-handler |GraphPatternNotTriples| down-pass) ;; default
(define-handler |TriplesBlock| up-pass down-pass) ;; perhaps better custom behaviour?
(define-handler |GroupOrUnionPattern| up-pass down-pass)
(define-handler |GroupOrUnionGraphPattern| down-pass up-options-pass)
(define-handler |OptionalGraphPattern| down-pass)
(define-handler |MinusGraphPattern| down-pass up-negative-pass)
(define-handler |GraphGrahpPattern| up-pass down-pass)
(define-handler |ServiceGraphPattern| up-pass down-pass)
(define-handler |Filter| up-pass down-pass)
(define-handler |Bind| down-pass)
(define-handler |InlineData| up-pass down-pass)

