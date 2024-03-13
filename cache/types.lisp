(in-package #:type-cache)

;;; hash table wrappers

(defun make-cache-table ()
  "Constructs new table."
  (lhash:make-castable :test #'equal))

(defun get-cache-hash (key table &optional default)
  "Gets key from table."
  (lhash:gethash key table default))

(defun (setf get-cache-hash) (value key table &optional default)
  (setf (lhash:gethash key table default) value))

(defun has-cache-hash (key table)
  "Yields truthy iff key exists in table."
  (multiple-value-bind (value knownp)
      (get-cache-hash key table nil)
    (declare (ignore value))
    knownp))

(defun rem-cache-hash (key table)
  "Removes key from table."
  (lhash:remhash key table))

;;; cache implementation

(defparameter *known-types-by-graphs*
  (make-cache-table)
  "Maps a graph to a castable containing a mapping from a uri to a list of its types.

Hence graph -> uri -> types in which every URI is expressed as a string and the graphs and types are lists of strings.")

;; The cache knows each graph in which a type is stored and knows which
;; graphs do _not_ contain the relevant types too.  The cache itself is
;; filled with types as they are discovered for each graph.  When types
;; are added, the graph is updated directly iff other types are already
;; known for that graph.  When types are removed they are removed iff
;; the types are already known.

(defun table-for-graph (graph)
  "Yields the castable matching the graphs."
  (if (has-cache-hash graph *known-types-by-graphs*)
      (get-cache-hash graph *known-types-by-graphs* nil)
      (setf (get-cache-hash graph *known-types-by-graphs*) (make-cache-table))))

(defun remove-cached-type (uri graph)
  "Removes the cached type for uri and graph."
  ;; TODO: ensure this is called whenever a quad is added or removed
  ;; regarding types.

  ;; TODO: optimize clearing by updating the cached types when we know
  ;; the cached types.
  (when-let ((uri-to-types-hash (get-cache-hash graph *known-types-by-graphs*)))
    (rem-cache-hash uri uri-to-types-hash)))

(defparameter *allow-live-type-cache-update* t
  "Allow the cache to update changes live.  This is executed without
   locking on the specific type but with checks, meaning there is a tiny
   time-window in which a concurrent update could dirty the cache.")

(defun push-types-to-existing-cache (uri graph types)
  "Pushes TYPES to teh known types for URI and GRAPH if and only if the types for URI are already known."
  (when-let* ((table (table-for-graph graph)))
    (multiple-value-bind (current-values values-p)
        (get-cache-hash uri table)
      (when values-p
        (setf (get-cache-hash uri table)
              (delete-duplicates (concatenate 'list current-values types) :test #'string=))))))

(defun update-known-types (&key deletes inserts)
  "Loops over deletes and inserts and updates the known cache based on the received values."
  (flet ((extract-uri-type-graph-combinations (quads)
           (loop for quad in quads
                 for predicate = (jsown:val (handle-update-unit::match-as-binding (getf quad :predicate)) "value")
                 when (string= predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   collect (list (jsown:val (handle-update-unit::match-as-binding (getf quad :subject)) "value")
                                 (jsown:val (handle-update-unit::match-as-binding (getf quad :object)) "value")
                                 (jsown:val (handle-update-unit::match-as-binding (getf quad :graph)) "value"))))
         (remove-overlap (left right)
           (values (set-difference left right :test #'equal)
                   (set-difference right left :test #'equal))))
    ;; 1. filter out what has to do with setting values
    (let ((delete-types (extract-uri-type-graph-combinations deletes))
          (insert-types (extract-uri-type-graph-combinations inserts)))
      ;; 2. convert to effective inserts and effective deletes
      (multiple-value-bind (effective-delete-types effective-insert-types)
          (remove-overlap delete-types insert-types)
        ;; 3. clear what needs to be cleared
        (loop for (uri type graph) in effective-delete-types
              do (remove-cached-type uri graph))
        ;; 4. add to those values which are suspected to be known; or
        ;; clear the changed ones
        (if *allow-live-type-cache-update*
            (loop for ((uri type graph) . rest)
                    in (support:group-by effective-insert-types
                                         #'equal
                                         :key (lambda (uri-type-graph)
                                                (cons (first uri-type-graph) (third uri-type-graph))))
                  do (push-types-to-existing-cache uri graph (cons type (mapcar #'second rest))))
            (loop for (uri type graph) in effective-delete-types
                  do (remove-cached-type uri graph)))))))

(defun cache-types-for-uri-and-graph (uri graph types)
  "Stores the supplied types as the known types for the given URI and GRAPH."
  (setf (get-cache-hash uri (table-for-graph graph))
        types))

(defun construct-query-ast-for-fetching-types (uris graphs)
  "Constructs a SPARQL query for fetching the types for the supplied URIs and GRAPHS."
  (flet ((update-inline-data-one-var-for-uris (inline-data-one-var uris)
           (setf (sparql-parser:match-submatches inline-data-one-var)
                 (destructuring-bind (var open-curly data-block-value close-curly)
                     (sparql-parser:match-submatches inline-data-one-var)
                   `(,var
                     ,open-curly
                     ,@(loop for uri in uris
                             collect (sparql-parser:make-match
                                      :term 'ebnf::|DataBlockValue|
                                      :rule (sparql-parser:match-rule data-block-value)
                                      :submatches (list (sparql-manipulation:make-iri uri))))
                     ,close-curly)))))
    (sparql-parser:with-parser-setup
      (let* ((ast
               (sparql-parser:parse-sparql-string
                (coerce "SELECT DISTINCT ?graph ?resource ?type WHERE { VALUES ?resource { <http://a> } VALUES ?graph { <http://b> } GRAPH ?graph { ?resource a ?type. } }"
                        #-be-cautious 'base-string #+be-cautious 'string)))
             (inline-data-one-var-nodes
               (sparql-manipulation::follow-path
                (sparql-parser:sparql-ast-top-node ast)
                '(ebnf::|QueryUnit|
                  (ebnf::|Query|
                   (ebnf::|SelectQuery|
                    (ebnf::|WhereClause|
                     (ebnf::|GroupGraphPattern|
                      (ebnf::|GroupGraphPatternSub|
                       (ebnf::|GraphPatternNotTriples|
                              (ebnf::|InlineData|
                                     (ebnf::|DataBlock|
                                            ebnf::|InlineDataOneVar|))))))))))))
        (update-inline-data-one-var-for-uris (first inline-data-one-var-nodes) uris)
        (update-inline-data-one-var-for-uris (second inline-data-one-var-nodes) graphs)
        (and (or (sparql-generator:is-valid ast) (error "We have generated an invalid ast for selecting types?!"))
             ast)
        ast))))

(defparameter *uri-graph-user-type-providers* nil
  "A list of functions that can calculate the types for a list of combined graph and uri.")

(defun derive-type-from-prefix-function (prefix types &optional complete-p)
  "Constructs a function that derives the types of uris starting with PREFIX.

If COMPLETE-P is non-nil processing may be short-circuited but no
guarantees are given for this to be used."
  (let ((prefix-length (length prefix)))
    (format t "~&Types is now: ~A~%" types)
    (lambda (uri graph)
      (declare (ignore graph))
      (format t "~&Types is now: ~A~%" types)
      (let ((uri-length (length uri)))
        (if (and (>= uri-length prefix-length)
                 (string= prefix (subseq uri 0 prefix-length)))
            (progn (format t "~&Returning types ~A for uri ~A~%"
                           types uri)
                   (values types complete-p))
            (values nil nil))))))

(defun derive-user-types (uri graph)
  "Derives the set of user specified types for URI and GRAPH."
  (let (derived-types)
    (loop for user-type-deriver in *uri-graph-user-type-providers*
          for (found-types complete-p) = (multiple-value-list (funcall user-type-deriver uri graph))
          do
             (alexandria:when-let ((new-types (concatenate 'list derived-types found-types)))
               (format t "~&Setting new derived types from ~A to ~A because of newly found types ~A~%"
                       derived-types new-types found-types)
               (setf derived-types new-types))
          when complete-p
            return (values derived-types t))
    (values derived-types nil)))

(defun add-type-for-prefix (prefix type &optional complete-p)
  "Ensures the type system derives that a URI starting with PREFIX should have type TYPE.

COMPLETE-P is to be understood as by DERIVE-TYPE-FROM-PREFIX-FUNCTION."
  (push (derive-type-from-prefix-function prefix (list type) complete-p)
        *uri-graph-user-type-providers*))

(defun fetch-types-for (uris graphs)
  "Fetches the types for the given URIs and GRAPHs."
  (let ((solutions-hash (make-hash-table :test 'equal))) ; regular non-concurrent hash table
    ;; this hash-table will have (graph . uri) as key.
    (client:batch-map-solutions-for-select-query
        ((construct-query-ast-for-fetching-types uris graphs)
         :for :fetch-types-for-insert
         :usage nil) ; setting usage to nil ensures access rights are not applied
        (bindings) 
      ;; initialize the solutions hash with the user-derived types so we
      ;; can emit empty results too
      (loop for uri in uris
            do (loop for graph in graphs
                     for user-types = (derive-user-types uri graph)
                     do
                        (setf (gethash (cons graph uri) solutions-hash)
                              user-types)))
      ;; augment the results with results from the triplestore
      (loop for binding in bindings
            for uri = (jsown:filter binding "resource" "value")
            for typeObj = (jsown:val binding "type")
            for graph = (jsown:filter binding "graph" "value")
            when (string= (jsown:val typeObj "type") "uri")
              do (push (jsown:val typeObj "value")
                       (gethash (cons graph uri) solutions-hash))))
    (loop for key being the hash-keys of solutions-hash
          for (graph . uri) = key
          collect `(,graph ,uri ,@(delete-duplicates (gethash key solutions-hash) :test #'string=)))))

(defun types-for (uris graphs)
  "Yields the types for the given URI, fetching the types if necessary."
  ;; This code may run concurrently hence we need to ensure to fetch all
  ;; relevant information and fill caches as best as we can.
  (let ((tables (mapcar #'table-for-graph graphs))
        (uri-graph-solutions (make-hash-table :test 'equal)) ; non-concurrent hash-table (uri . graph) => types
        missing-uri-graph-combinations)
    ;; discover which URIs are missing
    (loop for uri in uris
          do
             (loop for table in tables
                   for graph in graphs
                   for (found-types found-types-p)
                     = (multiple-value-list (get-cache-hash uri table))
                   if found-types-p
                     do
                        (setf (gethash (cons uri graph) uri-graph-solutions)
                              found-types)
                   else
                     do
                        (push (cons uri graph) missing-uri-graph-combinations)))
    ;; now find which uri graph combitations are missing
    ;; query all missing uri graph combinations
    (let* ((grouped-by-uri (support:group-by missing-uri-graph-combinations #'string= :key #'car))
           (uri-graphs-combinations (loop for ((uri . graph) . rest) in grouped-by-uri
                                          collect `(,uri ,graph ,@(mapcar #'cdr rest))))
           (grouped-by-graphs (support:group-by uri-graphs-combinations #'equal :key #'rest))
           (uris-graphs-combinations (loop for ((uri . graphs) . rest) in grouped-by-graphs
                                           collect `((,uri ,@(mapcar #'car rest))
                                                     .
                                                     ,graphs))))
      (loop for (uris . graphs) in uris-graphs-combinations
            do
               ;; push solutions into uri-graph-solutions
               (loop for (graph uri . types) in (fetch-types-for uris graphs)
                     do
                        (setf (gethash (cons uri graph) uri-graph-solutions) types) ; internal
                        (cache-types-for-uri-and-graph uri graph types)))) ; external reuse

    ;; yield flattened datastructure for uri-graph-solutions
    (loop for uri in uris
          collect
          (cons uri (delete-duplicates
                     (loop for graph in graphs
                           append (gethash (cons uri graph) uri-graph-solutions))
                     :test #'string=)))))
