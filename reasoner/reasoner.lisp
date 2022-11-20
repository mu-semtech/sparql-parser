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
         (extracted-info (extract-constraints query expanded-uris))
         (derivations (extract-derivation-tree query)))
    (values (derive-knowledge extracted-info derivations domain-model expanded-uris)
            prefixes
            expanded-uris
            extracted-info)))

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

(defun extract-prefixes (query)
  "Extract all prefixes from QUERY.
Assumes a fixed BASE is determined before it is used, as our query
processing should execute."
  ;; Extracts all prefixes
  (let ((answers (make-hash-table :test 'equal))
        (current-base nil))
    (flet ((extract-prefix-from-match (match)
             ;; PrefixDecl ::= 'PREFIX' PNAME_NS IRIREF
             (let* ((submatches (sparql-parser::match-submatches match))
                    (pname-ns-string (-> submatches
                                       (second)
                                       (sparql-parser:terminal-match-string)
                                       (pname-ns-strip-colon)))
                    (iriref-string (-> submatches
                                     (third)
                                     (sparql-parser:terminal-match-string)
                                     (iriref-string-strip-markers)
                                     (expand-uri current-base))))
               (setf (gethash pname-ns-string answers) iriref-string)))
           (extract-basedecl-from-match (match)
             ;; BaseDecl ::= 'BASE' IRIREF
             (setf current-base
                   (-> match
                     (sparql-parser:match-submatches)
                     (second)
                     (sparql-parser:terminal-match-string)
                     (iriref-string-strip-markers)))))
      (sparql-manipulation:loop-matches-symbol-case (match) query
        (ebnf::|PrefixDecl| (extract-prefix-from-match match))
        (ebnf::|BaseDecl| (extract-basedecl-from-match match)))
      (make-query-prefixes :prefix-hash answers :base current-base))))

(defun derive-expanded-uris (query)
  ;; Constructed expanded variant of each prefixed URI
  nil)

(defun extract-constraints (query prefixes)
  ;; Figures out which constraints are defined in the query.

  ;; Eg: a triple ?s a foaf:Person would derive indicate ?s must have an
  ;; rdf:type relationship to foaf:Person.  The expansions are used to
  ;; further extract information from these items.
  )

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
