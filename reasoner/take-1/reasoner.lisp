(in-package #:reasoner)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

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

(defun derived-knowledge (sparql-ast)
  ;; Extracts directly known knowledge from the sparql-ast.  This may derive
  ;; information from each triple.
  (let* ((reasoner-tree (construct-reasoner-ast sparql-ast))
         (prefixes (extract-prefixes reasoner-tree))
         (expanded-uris (derive-expanded-uris reasoner-tree prefixes))
         (extracted-info (extract-constraints reasoner-tree expanded-uris prefixes))
         ;; (derivations (extract-constraints reasoner-tree expanded-uris prefixes))
         )
    (values ;; (derive-knowledge extracted-info derivations domain-model expanded-uris)
            prefixes
            expanded-uris
            extracted-info
            ;; derivations
            )))

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

(defun tree-has-direct-term (reasoner-tree term)
  "Yields truethy iff REASONER-TREE's children contain submatch of term TERM."
  (loop for tree-item in (reasoner-ast-children reasoner-tree)
        for tree-term = (match-term (reasoner-ast-node tree-item))
        when (eq term tree-term)
          do (return t)
        finally (return nil)))

(defmacro term-case (match &body cases)
  "CASE on the MATCH-TERM of MATCH."
  `(case (sparql-parser:match-term ,match)
     ,@cases))

(defun extract-constraints (reasoner-tree match-uri-mapping prefixes)
  "Extract direct information from each part of the matches of QUERY, using
PREFIXES for the expanded URIs."
  ;; Figures out which constraints are defined in the query.

  ;; We intend to assign the knowledge to the upper-most EBNF node on
  ;; which we can define the knowledge.  This rule may allow us to make
  ;; a clear decision on where to position the knowledge.

  ;; Eg: a triple ?s a foaf:Person would derive indicate ?s must have an
  ;; rdf:type relationship to foaf:Person.  The expansions are used to
  ;; further extract information from these items.
  (labels ((process-subject (same-subject-tree)
             ;; same-subject-tree is an ebnf::|TriplesSameSubject| or
             ;; ebnf::|TriplesSameSubjectPath|
             (when (tree-has-direct-term same-subject-tree 'ebnf::|VarOrTerm|)
               (tree-scan-deep-term-case sub (same-subject-tree ebnf::|TriplesSameSubjectPath|)
                 ;; TODO: this will fail on RDFLiteral
                 (ebnf::|ABSTRACT-IRI| (process-subject-and-uri same-subject-tree sub))
                 (ebnf::|ABSTRACT-VAR| (process-subject-and-var same-subject-tree sub)))))
           (process-subject-and-uri (same-subject-tree tree-uri-node)
             ;; same-subject-match :: TriplesSameSubjectPath or TriplesSameSubjectPath
             ;; term :: Expandable URI for subject

             ;; DONE: Enrich predicate match
             ;; TODO: Drill down to predicate (and enrich uri with knowledge based on predicate)
             (with-named-child-tree (property-list-tree)
                 (same-subject-tree ebnf::|PropertyListPathNotEmpty|)
               (process-object :same-subject-tree same-subject-tree :subject-uri tree-uri-node)))
           (process-subject-and-var (same-subject-tree tree-var-node)
             ;; same-subject-match :: TriplesSameSubjectPath or TriplesSameSubject
             ;; term :: VAR for subject

             ;; TODO: Drill down to predicate (and enrich uri with knowledge based on predicate)
             ;; (format t "Working through ~A ~A" same-subject-match term)
             (with-named-child-tree (property-list-tree)
                 (same-subject-tree ebnf::|PropertyListPathNotEmpty|)
               (process-object :same-subject-tree same-subject-tree :subject-var tree-var-node)))
           (process-object (&key same-subject-tree subject-uri subject-var)
             ;; same-subject-tree :: TriplesSameSubjectPath or TriplesSameSubject
             ;; subject-uri :: ABSTRACT-URI for subject or nil
             ;; subject-var :: ABSTRACT-VAR for subject or nil

             ;; (format nil "process-object got called with ~A ~A ~A" same-subject-match subject-uri subject-var)
             (with-named-child-tree (child-tree) (same-subject-tree ebnf::|PropertyListPathNotEmpty|)
               (do-grouped-tree-children (predicate-path-tree objects-tree)
                   ; (do-grouped-tree-children (predicate-path objects-match)
                   (child-tree :amount 2
                               :filter-terms (ebnf::|VerbPath| ebnf::|VerbSimple| ebnf::|ObjectList| ebnf::|ObjectListPath|)
                               :error-on-incomplete-amount-p t)
                 ;; ObjectList or ObjectListPath
                 ;; these objects will contain extra information
                 (do-grouped-tree-children (single-object-tree)
                     (objects-tree :filter-terms (ebnf::|ObjectPath| ebnf::|Object|))
                   ;; TODO: Correctly support or break in case of RDFLiteral
                   (let ((properties (list same-subject-tree :predicate predicate-path-tree)))
                     (when subject-var
                       (alexandria:appendf properties (list :left-var subject-var)))
                     (when subject-uri
                       (alexandria:appendf properties (list :left-uri subject-uri)))
                     (tree-scan-deep-term-case sub (single-object-tree ebnf::|TriplesSameSubjectPath|)
                       (ebnf::|ABSTRACT-IRI| (alexandria:appendf properties (list :right-uri sub)))
                       (ebnf::|ABSTRACT-VAR| (alexandria:appendf properties (list :right-var sub)))
                       (ebnf::|ABSTRACT-PRIMITIVE| (alexandria:appendf properties (list :right-primitive sub))))
                     (apply (function extract-info) properties))))))
           (extract-info (tree &key left-var left-uri predicate right-var right-uri right-primitive)
             (let ((predicate-node (reasoner-ast-node predicate)))
               (when left-var (setf (node-knowledge predicate-node :left-var) (reasoner-ast-node left-var)))
               (when left-uri (setf (node-knowledge predicate-node :left-uri) (reasoner-ast-node left-uri)))
               (when right-var (setf (node-knowledge predicate-node :right-var) (reasoner-ast-node right-var)))
               (when right-uri (setf (node-knowledge predicate-node :right-uri) (reasoner-ast-node right-uri)))
               (when right-primitive (setf (node-knowledge predicate-node :right-primitive) (reasoner-ast-node right-primitive)))
               ;; TODO: support predicate as a real path instead, it may currently be complex
               (with-known-local-prefixes (:prefixes prefixes :uri-mapping match-uri-mapping)
                 (reasoner-term-info:add-subject-predicate-object tree
                                                                  (or left-var left-uri)
                                                                  predicate
                                                                  (or right-var right-uri right-primitive))))))
    (reasoner-tree-mirror:loop-tree-matches-symbol-case (tree) reasoner-tree
      ;; Interpret subject
      ;; Drill down for predicate and object
      (ebnf::|TriplesSameSubjectPath| (process-subject tree))
      (ebnf::|TriplesSameSubject| (process-subject tree)))))

;; These properties can be used to push information through various levels
(defparameter handler-pass-had-change nil
  "Special case indicating whether the pass yielded a change of information or not.")

(defun handle-peers-pass (tree)
  "Passes upstream knowledge to peers, and from peers to peers."
  (let* ((subtrees (reasoner-tree-mirror:reasoner-ast-children tree))
         (known-info (apply #'union-term-info (cons tree subtrees))))
    (dolist (subtree subtrees)
      (setf (term-info subtree)
            known-info))))

(defun handle-down-pass (tree)
  "Passes knowledge from the current tree to its children."
  (dolist (subtree (reasoner-tree-mirror:reasoner-ast-children tree))
    (setf (term-info subtree) (union-term-info tree subtree))))

(defun handle-up-pass (tree)
  "Passes knowledge from the match's children to the current match."
  (let* ((children (reasoner-tree-mirror:reasoner-ast-children tree))
         (term-union (apply #'union-term-info (cons tree children))))
    (setf (term-info tree) term-union)))

(defun handle-up-options-pass (tree)
  "Passes knowledge from the match's children to the parent as a series of options."
  ;; find all option clauses from children
  ;; combine them x with parent
  (setf (term-info tree)
        (apply #'union-term-info tree (reasoner-tree-mirror:reasoner-ast-children tree))))

(defun handle-up-negative-pass (tree)
  "Constructs a negative pass, indicating the following is something that would *not* resolve."
  (declare (ignore tree))
  ;; TODO: construct negative mach for MINUS
  nil)

(defgeneric handle-custom-term (term tree)
  (:documentation "Handles a custom approach for the given match.")
  (:method ((term (eql 'ebnf::|TriplesBlock|)) (tree reasoner-tree-mirror:reasoner-ast))
    ;; Unless calculated before, we should extract all available information.
    (format t "~&Handling custom term ~A for match ~&~A~&" term tree))
  (:method (term (tree reasoner-tree-mirror:reasoner-ast))
    ;; Unless calculated before, we should extract all available information.
    (format t "~&No term custom matcher found for ~A for REASONER-TREE-MIRROR:REASONER-AST~%" term)))

(defun handle-custom (tree)
  (handle-custom-term (sparql-parser:match-term (reasoner-tree-mirror:reasoner-ast-node tree))
                      tree))

(defun tree-distribution-approaches (tree &optional default)
  "Returns the distribution approaches for TREE."
  (getf *information-distribution-approaches*
        (match-term (reasoner-tree-mirror:reasoner-ast-node tree))
        default))

(defun walk-distribute-match-1 (tree &key (process-match-p (lambda (match) (declare (ignore match)) t)))
  "Runs all distribution efforts for MATCH once.

PROCESS-MATCH-P is called for each match to determine if it should be
processed.  We also descend for discribution when this is false."
  (when (funcall process-match-p tree)
    (dolist (processor (tree-distribution-approaches tree (list #'handle-down-pass)))
      (funcall processor tree)))
  (dolist (subtree (reasoner-tree-mirror:reasoner-ast-children tree))
    (walk-distribute-match-1 subtree :process-match-p process-match-p)))

(defun iterate-distribution-walk (tree &optional tracker)
  "Iterates through the distribution walking for any element that may need updating.

When tracker is supplied, it should be a TERM-INFO-TRACKER which
contains currently interesting items to track."
  (let ((last-tracker nil)
        (current-tracker tracker))
    (loop for i from 0 below 1000 ;; max 1000 iterations
          do
             (setf last-tracker current-tracker)
             (with-term-info-change-tracking
               (walk-distribute-match-1 tree
                                        :process-match-p
                                        (lambda (tree)
                                          (or (not last-tracker)
                                              (some (rcurry #'term-info-tracking-contains last-tracker)
                                                    (cons tree (reasoner-tree-mirror:reasoner-ast-children tree))))))
               ;; (format t "~&In iteration ~A we had ~A changes.~%" i (reasoner-term-info::term-info-tracking-tracked-amount))
               (setf current-tracker (term-info-tracking-get-current-tracker)))
          until (term-info-tracking-empty-p current-tracker))))

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

