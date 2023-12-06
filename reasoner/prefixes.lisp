(in-package #:reasoner-prefixes)

;;;; Prefixes extraction
;;;;
;;;; Many URIs are expressed using prefixes.  This module extracts
;;;; perfixes from a query and provides a cache-based variant to use
;;;; them.
;;;;
;;;; This module is not thread-safe and assumes prefixes are constructed
;;;; used in a single thread at any time.  The module can be used in
;;;; multiple threads when ran in a separate WITH-LOCAL-PREFIXES block.

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(declaim (special *prefixes* *match-uri-mapping*))

(defmacro with-known-local-prefixes ((&key
                                        (prefixes '(make-equery-prefixes))
                                        (uri-mapping '(make-hash-table :test 'eq)))
                                     &body body)
  "Runs BODY within a context where PREFIXES and URI-MAPPING are bound
correctly for the functions in this module."
  `(let ((*prefixes* ,prefixes)
         (*match-uri-mapping* ,uri-mapping))
     ,@body))

(defmacro with-local-prefixes (&body body)
  `(with-known-local-prefixes ()
     ,@body))

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

(defun extract-prefixes (reasoner-ast)
  "Extract all prefixes from REASONER-AST.
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
      (loop-tree-matches-symbol-case (ast) reasoner-ast
        (ebnf::|PrefixDecl| (extract-prefix-from-match (reasoner-ast-node ast)))
        (ebnf::|BaseDecl| (extract-basedecl-from-match (reasoner-ast-node ast))))
      (make-query-prefixes :prefix-hash answers :base current-base))))

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

(defun derive-expanded-uris (ast prefixes)
  "Expands all prefixed matches of QUERY based on PREFIXES."
  (let ((match-uri-mapping (make-hash-table :test 'eq)))
    ;; TODO: we could skip those mentioned in PREFIXES by extending the tooling
    (loop-tree-matches-symbol-case (tree) ast
      (ebnf::|PNAME_LN| (cached-expanded-uri (reasoner-ast-node tree) :prefixes prefixes :match-uri-mapping match-uri-mapping))
      (ebnf::|PNAME_NS| (cached-expanded-uri (reasoner-ast-node tree) :prefixes prefixes :match-uri-mapping match-uri-mapping)))
    match-uri-mapping))
