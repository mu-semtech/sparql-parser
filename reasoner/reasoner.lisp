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

(defun construct-derivation-tree (query)
  ;; Should return rules on propagating what we've derived.
  nil)

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

(defmacro traverse-query-terms ((var) query &body body)
  body)

(defun iriref-string-strip-markers (string)
  (if (and (char= (elt string 0) #\<)
           (char= (elt string (1- (length string))) #\>))
      (coerce (subseq string 1 (1- (length string))) 'base-string)
      (error "Cannot strip iriref string markers from STRING it does not have any, for: ~A" string)))

(defun pname-ns-strip-colon (string)
  (if (char= (elt string (1- (length string))) #\:)
      (coerce (subseq string 0 (1- (length string))) 'base-string)
      (error "Cannot strip pname-ns colon from STRING it does not end with colon, for: ~A" string)))

(defstruct query-prefixes
  (prefix-hash (make-hash-table :test 'equal))
  (base (coerce "http://mu.semte.ch/local/" 'base-string) :type base-string))

(defun get-prefix (query-prefixes prefix)
  "Gets a prefix from the query-prefixes information."
  (gethash prefix (query-prefixes-prefix-hash query-prefixes)))

(defun (setf get-prefix) (value query-prefixes prefix)
  (setf (gethash prefix (query-prefixes-prefix-hash query-prefixes)) value))

(defun expand-uri (uri-string base)
  "Expands URI-STRING with respect to BASE."
  ;; TODO: expand prefixes based on current BASE
  (declare (ignore base))
  uri-string)

(defmacro with-named-child ((var) (match term) &body body)
  "Executes BODY in a context where VAR is bound to the first submatch of
MATCH that has symbol TERM.  If no solution is found BODY is not
executed and NIL is returned."
  `(alexandria:when-let ((,var (find ',term (sparql-parser:match-submatches ,match) :test (lambda (term match) (and (sparql-parser:match-p match) (eq term (sparql-parser:match-term match)))))))
     ,@body))

(defun extract-prefixes (query)
  "Extract all prefixes from QUERY.
Assumes a fixed BASE is determined before it is used, as our query
processing should execute."
  ;; Extracts all prefixes
  (let ((answers (make-hash-table :test 'equal))
        (current-base nil))
    (flet ((extract-prefix-from-match (match)
             ;; PrefixDecl ::= 'PREFIX' PNAME_NS IRIREF
             (do-grouped-children (pname-ns iriref)
                 (match :amount 2 :filter-terms (ebnf::|PNAME_NS| ebnf::|IRIREF|))
               (let ((pname-ns-string (-> pname-ns
                                        (sparql-parser:terminal-match-string)
                                        (pname-ns-strip-colon)))
                     (iriref-string (-> iriref
                                      (sparql-parser:terminal-match-string)
                                      (iriref-string-strip-markers)
                                      (expand-uri current-base))))
                 (setf (gethash pname-ns-string answers) iriref-string))))
           (extract-basedecl-from-match (match)
             ;; BaseDecl ::= 'BASE' IRIREF
             (with-named-child (iriref)
                 (match ebnf::|IRIREF|)
               (setf current-base
                     (-> iriref
                       (sparql-parser:terminal-match-string)
                       (iriref-string-strip-markers))))))
      (loop-matches-symbol-case (match) query
        (ebnf::|PrefixDecl| (extract-prefix-from-match match))
        (ebnf::|BaseDecl| (extract-basedecl-from-match match)))
      (make-query-prefixes :prefix-hash answers :base current-base))))

(declaim (special *prefixes* *match-uri-mapping*))
(defun (setf cached-expanded-uri) (uri-string match &key (prefixes *prefixes*) (match-uri-mapping *match-uri-mapping*))
  "Sets the CACHED-EXPANDED-URI for MATCH to URI-STRING and returns (coerced) URI-STRING."
  (declare (ignore prefixes))
  (setf (gethash match match-uri-mapping)
        (coerce uri-string 'base-string)))

(defun cached-expanded-uri (match &key (prefixes *prefixes*) (match-uri-mapping *match-uri-mapping*))
  "Yields the expanded URI for MATCH, given PREFIXES, caching it if it is not known yet."
  (let ((term (sparql-parser:match-term match)))
    (if (eq term 'ebnf::|IRIREF|)
        (iriref-string-strip-markers (sparql-parser:terminal-match-string match))
        (or (gethash match match-uri-mapping)
            (flet ((set-uri-mapping (value)
                     (setf (cached-expanded-uri match :prefixes prefixes :match-uri-mapping match-uri-mapping)
                           value)))
              (case term
                (ebnf::|PNAME_LN|
                 (destructuring-bind (prefix following)
                     (cl-utilities:split-sequence
                      #\: (sparql-parser:terminal-match-string match)
                      :count 2) ; TODO: cope with #\: in PN_LOCAL
                   (cond ((string= prefix "")
                          (set-uri-mapping (query-prefixes-base prefixes)))
                         ((get-prefix prefixes prefix)
                          (set-uri-mapping (concatenate 'string
                                                        (get-prefix prefixes prefix)
                                                        following)))
                         (t (error "Missing prefix ~A" prefix)))))
                (ebnf::|PNAME_NS|
                 (let* ((matched-string (sparql-parser:terminal-match-string match))
                        ;; cut off the : at the end
                        (prefix (subseq matched-string 0 (1- (length matched-string)))))
                   (cond ((string= prefix "")
                          (set-uri-mapping (query-prefixes-base prefixes)))
                         ((get-prefix prefixes prefix)
                          (set-uri-mapping (get-prefix prefixes prefix)))
                         (t (error "Missing prefix ~A" prefix)))))))))))

(defun derive-expanded-uris (query prefixes)
  "Expands all prefixed matches of QUERY based on PREFIXES."
  (let ((match-uri-mapping (make-hash-table :test 'eq)))
    ;; TODO: we could skip those mentioned in PREFIXES by extending the tooling
    (loop-matches-symbol-case (match) query
      (ebnf::|PNAME_LN| (cached-expanded-uri match :prefixes prefixes :match-uri-mapping match-uri-mapping))
      (ebnf::|PNAME_NS| (cached-expanded-uri match :prefixes prefixes :match-uri-mapping match-uri-mapping)))
    match-uri-mapping))

(defun extract-triple-sets (query match-uri-mapping prefixes)
  "Extracts triples from QUERY with some understanding of how they're
related."
  )

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
  (let (uris variables)
   (labels ((process-subject (subject-match)
              (when (match-has-direct-term subject-match 'ebnf::|VarOrTerm|)
                (sparql-manipulation::scan-deep-term-case sub (subject-match ebnf::|TriplesSameSubjectPath|)
                  (ebnf::|ABSTRACT-IRI| (process-subject-and-uri subject-match sub))
                  (ebnf::|ABSTRACT-VAR| (process-subject-and-var subject-match sub)))))
            (process-subject-and-uri (subject-match term)
              ;; subject-match :: TriplesSameSubjectPath
              ;; term :: Expandable URI for subject

              ;; DONE: Enrich predicate match
              ;; TODO: Drill down to predicate (and enrich uri with knowledge based on predicate)
              (let ((iri (cached-expanded-uri term
                                              :prefixes prefixes
                                              :match-uri-mapping match-uri-mapping)))
                (push iri uris) ; TODO: for fun, remove
                (with-named-child (property-list-match) (subject-match ebnf::|PropertyListPathNotEmpty|)
                  (setf (node-knowledge property-list-match :left-iri) iri) ; TODO: can move to one of the specific cases
                  (process-object :subject-path subject-match :subject-uri iri))))
            (process-subject-and-var (match term)
              ;; TODO: Drill down to predicate (and enrich uri with knowledge based on predicate)
              ;; (format t "Working through ~A ~A" match term)
              (with-named-child (property-list-match) (match ebnf::|PropertyListPathNotEmpty|)
                (let ((var-string (sparql-parser:scanned-token-effective-string (first (sparql-parser:match-submatches term)))))
                  (push var-string variables) ; TODO: for fun, remove
                  (process-object :subject-path match :subject-var var-string)))) 
            (extract-match-uri-mapping (match)
              (cached-expanded-uri match
                                   :prefixes prefixes
                                   :match-uri-mapping match-uri-mapping))
            (extract-match-string (match)
              (sparql-parser:scanned-token-effective-string (first (sparql-parser:match-submatches match))))
            (process-object (&key subject-path subject-uri subject-var)
              (format nil "process-object got called with ~A ~A ~A" subject-path subject-uri subject-var)
              (with-named-child (child) (subject-path ebnf::|PropertyListPathNotEmpty|)
                (prog1 child)
                (do-grouped-children (predicate-path objects-match)
                    (child :amount 2
                           :filter-terms (ebnf::|VerbPath| ebnf::|VerbSimple| ebnf::|ObjectList| ebnf::|ObjectListPath|)
                           :error-on-incomplete-amount-p t)
                  ;; ObjectList or ObjectListPath
                  ;; these objects will contain extra information
                  ;; (format t "Got ~A" objects-match)
                  (prog1 objects-match)
                  (do-grouped-children (single-object-match) (objects-match :filter-terms (ebnf::|ObjectPath| ebnf::|Object|))
                    (prog1 single-object-match)
                    ;; TODO: Correctly support or break in case of RDFLiteral
                    (sparql-manipulation::scan-deep-term-case sub (single-object-match ebnf::|TriplesSameSubjectPath|)
                      (ebnf::|ABSTRACT-IRI| (if subject-var
                                                (extract-info-from-var-pred-uri subject-var
                                                                                predicate-path
                                                                                (extract-match-uri-mapping sub))
                                                (extract-info-from-uri-pred-uri subject-uri
                                                                                predicate-path
                                                                                (extract-match-uri-mapping sub))))
                      (ebnf::|ABSTRACT-VAR| (if subject-var
                                                (extract-info-from-var-pred-var subject-var
                                                                                predicate-path
                                                                                (extract-match-string sub))
                                                (extract-info-from-uri-pred-var subject-uri
                                                                                predicate-path
                                                                                (extract-match-string sub))))
                      (ebnf::|ABSTRACT-PRIMITIVE| (if subject-var
                                                      (extract-info-from-var-pred-term subject-var
                                                                                       predicate-path
                                                                                       (extract-match-string sub))
                                                      (extract-info-from-uri-pred-term subject-uri
                                                                                       predicate-path
                                                                                       (extract-match-string sub)))))))))
            ;; (sparql-parser:scanned-token-effective-string (first (sparql-parser:match-submatches term)))
            (extract-info-from-var-pred-var (left predicate right)
              (setf (node-knowledge predicate :left-var) left)
              (push right (node-knowledge predicate :right-var))
              (format t "~&Var pred var :: ~A ~A ~A~%" left (sparql-generator::write-valid-match predicate) right))
            (extract-info-from-var-pred-uri (left predicate right)
              (setf (node-knowledge predicate :left-var) left)
              (push right (node-knowledge predicate :right-uri))
              (format t "~&Var pred uri :: ~A ~A ~A~%" left (sparql-generator::write-valid-match predicate) right))
            (extract-info-from-uri-pred-var (left predicate right)
              (setf (node-knowledge predicate :left-uri) left)
              (push right (node-knowledge predicate :right-var))
              (format t "~&uri pred var :: ~A ~A ~A~%" left (sparql-generator::write-valid-match predicate) right))
            (extract-info-from-uri-pred-uri (left predicate right)
              (setf (node-knowledge predicate :left-uri) left)
              (push right (node-knowledge predicate :right-uri))
              (format t "~&uri pred uri :: ~A ~A ~A~%" left (sparql-generator::write-valid-match predicate) right))
            (extract-info-from-var-pred-term (left predicate right)
              (setf (node-knowledge predicate :left-var) left)
              (push right (node-knowledge predicate :right-primitive))
              (format t "~&var pred term :: ~A ~A ~A~%" left (sparql-generator::write-valid-match predicate) right))
            (extract-info-from-uri-pred-term (left predicate right)
              (setf (node-knowledge predicate :left-uri) left)
              (push right (node-knowledge predicate :right-primitive))
              (format t "~&uri pred term :: ~A ~A ~A~%" left (sparql-generator::write-valid-match predicate) right)))
     (loop-matches-symbol-case (match) query
       ;; Interpret subject
       ;; Drill down for predicate and object
       (ebnf::|TriplesSameSubjectPath| (process-subject match))
       (ebnf::|TriplesSameSubject| (process-subject match))))
    (list uris variables)))

(defun extract-derivation-tree (query)
  ;; Figures out which dependencies are within the SPARQL bnf.

  ;; Eg: a subject that's been reused must have the same constraints
  ;; applied to it as other elements.  If some place says ?s a
  ;; foaf:Person and another place says ?s ext:level ?over9000, then we
  ;; know the ?over9000 must be related to a foaf:Person.  This is not
  ;; bidirectional in all cases and the EBNF may not be the easiest to
  ;; reason on.
  )

(defun derive-knowledge (extracted-info derivations domain-model expanded-uris)
  ;; Extracts all knowledge from the tree based on the logical knowledge
  ;; we have of the world.  This entails learning more and more
  ;; information from the model until we've ran into a fixpoint in which
  ;; there is no new knowledge learned.
  )
