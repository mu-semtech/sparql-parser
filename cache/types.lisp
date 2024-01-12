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

(defun fetch-types-for (uris graphs)
  "Fetches the types for the given URIs and GRAPHs."
  (let ((solutions-hash (make-hash-table :test 'equal))) ; regular non-concurrent hash table
    ;; this hash-table will have (graph . uri) as key.
    (client:batch-map-solutions-for-select-query
        ((construct-query-ast-for-fetching-types uris graphs)
         :for :fetch-types-for-insert
         :usage nil) ; setting usage to nil ensures access rights are not applied
        (bindings) 
      ;; initialize the solutions hash to be empty so we can emit empty results too
      (loop for uri in uris
            do (loop for graph in graphs
                     do (setf (gethash (cons graph uri) solutions-hash) nil)))
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
        (uri-graph-solutions (make-hash-table :test 'equal)) ; non-concurrent hash-table (graph . uri) => types
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
