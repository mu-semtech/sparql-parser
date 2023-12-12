(in-package #:acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")

(defclass access ()
  ((name :initform (error "Must supply NAME when defining access.")
         :initarg :name
         :reader name))
  (:documentation "Specifies how a thing can be accessed."))

(defgeneric access-string-name (access)
  (:documentation "String variant of the name of the access right.")
  (:method ((access access))
    (name access)
    ;; (string-downcase (symbol-name (name access)))
    ))

(defun find-access-by-name (name)
  "Find access by name."
  (find name *access-specifications*
        :test #'string=
        :key #'access-string-name))

(defclass access-by-query (access)
  ((query :initform (error "Query is required to be specified for access-by-query")
          :initarg :query
          :reader query)
   (variables :initform nil
              :initarg :vars
              :reader variables))
  (:documentation "Access determined by a query yielding results."))

(defmethod initialize-instance :after ((access access-by-query) &key &allow-other-keys)
  (sparql-parser:with-parser-setup
    (setf (slot-value access 'query)
          (sparql-parser:parse-sparql-string (coerce (query access) #-be-cautious 'base-string #+be-cautious 'string)))))

(defclass always-accessible (access)
  ()
  (:documentation "An entity which is always accessible.  Used for public resources."))

(defstruct access-token
  (access (error "Must supply ACCESS when creating ACCESS-TOKEN") :type access-grant)
  (parameters nil :type list))

(defgeneric calculate-access-tokens (access &key mu-session-id)
  (:documentation "Yields truethy iff the given access is accessible within the current access context.")
  (:method ((access always-accessible) &key mu-session-id)
    (declare (ignore mu-session-id))
    (mapcar (lambda (grant) (make-access-token :access grant))
            (access-grants-for-access-name (name access))))
  (:method ((access access-by-query) &key mu-session-id)
    (when mu-session-id ; ignore when no mu-session-id supplied
      (let ((copied-ast (sparql-parser:make-sparql-ast
                      :string (sparql-parser:sparql-ast-string (query access))
                      :top-node (sparql-parser:copy-match
                                 (sparql-parser:sparql-ast-top-node (query access))
                                 t))))
        (sparql-parser:with-sparql-ast copied-ast
          (let ((new-query (replace-iriref copied-ast :from "SESSION_ID" :to mu-session-id)))
            (unless (sparql-generator:is-valid new-query)
              (error "Generated invalid access query"))
            (loop for binding
                    in (-> new-query
                         (sparql-generator:write-valid)
                         (client:query)
                         (client:bindings))
                  append
                  (mapcar (lambda (grant)
                            (make-access-token
                             :access grant
                             :parameters (mapcar (lambda (var) (jsown:filter binding var "value"))
                                                 (variables access))))
                          (access-grants-for-access-name (name access))))))))))

(defun access-token-jsown (token)
  "Yields a jsown representation of the access token."
  (jsown:new-js
    ("name" (access-grant-access (access-token-access token)))
    ("variables" (access-token-parameters token))))

(defun jsown-access-token (jsown)
  "Yields an ACCESS-TOKEN from the JSOWN specification."
  (make-access-token :access (find-access-grant-by-name (jsown:val jsown "name"))
                     :parameters (jsown:val jsown "variables")))

;; NOTE: the approach we take here to Constraints is insufficient.  A
;; skos:ConceptScheme with skos:Concept resources is a clear example of
;; paths which would need to be followed instead of determining the
;; location based on a single triple.  Better basic constructs should be
;; discovered for experimenting with such purposes.
(defstruct graph-specification
  (name (error "Must supply name to graph name") :type symbol)
  (base-graph (error "Must supply base graph string") :type string)
  ;; NOTE: the constraints are currently a list of triple-constraints
  ;; which means there is a lot of repetition in them.  This approach is
  ;; computationally rather intensive, but it would lead to a feasible
  ;; optimization using matrix calculation both for extra information
  ;; which we would need to discover as well as for calculating the
  ;; applicable constraints.  Check https://github.com/quil-lang/magicl
  ;; and https://quickref.common-lisp.net/3d-matrices.html#g_t_276828_2769
  ;; in that case.
  (constraints nil))

(defstruct access-grant
  (usage (list :read))
  (graph-spec (error "Must supply graph spec") :type symbol)
  (access (error "Must supply which grant allows access") :type string))

(defun access-grants-for-access-name (name)
  "Yields a list of all access grants from *rights* which have NAME as ACCESS-GRANT-ACCESS."
  (remove-if-not (lambda (grant) (string= (access-grant-access grant) name))
                 *rights*))

(defparameter *active-access-rights* nil
  ;; TODO: currently not used.  Not sure if this should be globally
  ;; available as contextual information.
  "List of the current call's access rights.")

(defun find-access-grant-by-name (name)
  "Find access right by name."
  (find name *rights*
        :test #'string=
        :key #'access-grant-access))

(defun access-tokens-from-allowed-groups (mu-auth-allowed-groups)
  "Calculates access rights formthe mu-auth-allowed-groups string."
  (loop for group in (if (stringp mu-auth-allowed-groups)
                         (jsown:parse mu-auth-allowed-groups)
                         mu-auth-allowed-groups)
        for name = (jsown:val group "name")
        for parameters = (jsown:val group "variables")
        append (loop for access-grant in (access-grants-for-access-name name)
                     collect (make-access-token
                              :access access-grant
                              :parameters parameters))))

(defun access-tokens-from-session-id (mu-session-id)
  "Calculates the access rights from the mu-session-id."
  (loop for access in *access-specifications*
        append (calculate-access-tokens access :mu-session-id mu-session-id)))

(defmacro with-test-code-json-access-tokens ((json-token-string) &body body)
  `(let ((*test-code-access-tokens* (access-tokens-from-allowed-groups ,json-token-string)))
     ,@body))

(defun calculate-and-cache-access-tokens (mu-auth-allowed-groups mu-session-id)
  "Calculates the access rights based on the mu-auth-allowed-groups string or mu-session-id."
  (if mu-auth-allowed-groups
      (access-tokens-from-allowed-groups mu-auth-allowed-groups)
      (let ((tokens (access-tokens-from-session-id mu-session-id)))
        (setf (mu-auth-allowed-groups)
              (jsown:to-json (mapcar #'access-token-jsown tokens)))
        tokens)))

(defmacro with-access-tokens ((access-tokens-var) &body body)
  "Executes BODY with ACCESS-RIGHTS-VAR bound to access rights for current
context.  Calculates access rights as needed and updates necessary
variables."
  `(let ((,access-tokens-var (calculate-and-cache-access-tokens (mu-auth-allowed-groups) (mu-session-id))))
     ,@body))

(defun accessible-graphs-with-tokens (tokens usage)
  "Yields a list of (CONS TOKEN GRAPH-SPECIFICATION) for the set of supplied tokens."
  (loop for token in tokens
        for token-name = (access-grant-access (access-token-access token))
        append (loop for right in *rights*
                     when (and (eq (access-grant-access right) token-name)
                               (find usage (access-grant-usage right) :test #'eq))
                       append (loop for graph-specification in *graphs*
                                    for granted-graph-spec-name = (access-grant-graph-spec right)
                                    when (eq granted-graph-spec-name
                                             (graph-specification-name graph-specification))
                                      collect (cons token graph-specification)))))

(defun graphs-for-tokens (tokens usage)
  "Yields the graphs which can be accessed from TOKENS."
  (loop for (token . graph-specification) in (accessible-graphs-with-tokens tokens usage)
        collect (token-graph-specification-graph token graph-specification)))

(defun token-graph-specification-graph (token graph-specification)
  "Assuming token belongs to graph-specification, yields the corresponding graph URL."
  (format nil "~A~{~A~,^/~}"
          (graph-specification-base-graph graph-specification)
          (access-token-parameters token)))

(defun apply-access-rights (query-ast &key (usage :read))
  "Applies current access rights to MATCH.

MATCH may be updated in place but updated MATCH is returned."
  (if (mu-auth-sudo)
      query-ast
      (sparql-parser:with-sparql-ast query-ast
        (with-access-tokens (tokens)
          (-> query-ast
            (remove-dataset-clauses)
            (remove-graph-graph-patterns)
            (add-default-base-decl-to-prologue)
            (add-from-graphs (or (graphs-for-tokens tokens usage)
                                 (list "http://mu-authorization.service.semantic.works/empty-graph"))))))))

(defmacro do-graph-constraint ((graph-constraint &optional (collection 'do)) (position kind value) &body body)
  "Executes BODY on each constraint of GRAPH-CONSTRAINT optionally collecting through COLLECTION and filtering on KIND.
  
- POSITION is a variable that will be bound to one of :subject :predicate :object :graph.
- KIND is either a variable that will be bound to the KIND of constraint or otherwise a keyword (being :VALUE or :TYPE) to be used as a constraint.
- VALUE is a variable that will be bound to the specific VALUE."
  (let ((kind-sym (if (keywordp kind) (gensym "KIND") kind)))
    `(loop for (,position (,kind-sym ,value)) on ,graph-constraint by #'cddr
           ,@(if (keywordp kind) `(when (eq ,kind ,kind-sym)))
           ,collection ,@body)))

(defun dispatch-quads (quads)
  "Applies current access rights to quads and updates them to contain the
desired graphs."
  (let ((known-type-uri-index (make-hash-table :test 'equal))
        (types-to-fetch (make-hash-table :test 'equal))
        (dispatched-quads nil))
    ;; The type is either T (it has this type) NIL (it does not have
    ;; this type) or it is not set (and therefore unknown)
    (labels
        ((set-known-type (uri type yes-no)
           ;; sets a type to be known in the type index
           (multiple-value-bind (table-for-type table-for-type-p)
               (gethash type known-type-uri-index (make-hash-table :test 'equal))
             (unless table-for-type-p
               (setf (gethash type known-type-uri-index) table-for-type))
             (setf (gethash uri table-for-type) yes-no)))
         (ensure-future-type-known (uri type)
           "Ensures the type will be known in the future"
           (multiple-value-bind (type-hash found-typehash-p)
               (gethash type known-type-uri-index)
             (unless (and found-typehash-p
                          (second (multiple-value-list (gethash uri type-hash))))
               (setf (gethash uri types-to-fetch) t))))
         (fetch-types-to-fetch ()
           "Fetches the types to fetch and populates the known-type-uri-index."
           (alexandria:when-let*
               ((types-to-fetch (alexandria:hash-table-keys types-to-fetch))
                (query
                 (sparql-parser:with-parser-setup
                   (let* ((ast
                            (sparql-parser:parse-sparql-string
                             (coerce "SELECT ?graph ?resource ?type WHERE { VALUES ?resource { <http://a> } GRAPH ?graph { ?resource a ?type. } }"
                                     #-be-cautious 'base-string #+be-cautious 'string)))
                          (inline-data-one-var
                            (first
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
                                                   (ebnf::|DataBlock|))))))))))))
                          (data-submatches (sparql-parser:match-submatches inline-data-one-var))
                          (data-block-value (third data-submatches)))
                     (setf (sparql-parser:match-submatches inline-data-one-var)
                           `(,(first data-submatches)
                             ,(second data-submatches)
                             ,@(loop
                                 for resource in types-to-fetch
                                 collect (sparql-parser::make-match
                                          :term 'ebnf::|DataBlockValue|
                                          :rule (sparql-parser:match-rule data-block-value)
                                          :submatches (list (make-iri resource))))
                             ,(fourth data-submatches)))
                     (and (sparql-generator:is-valid ast)
                          ast)))))
             (client:batch-map-solutions-for-select-query (query :for :fetch-types-for-insert :usage :read) (bindings)
                                        ; note that :usage :read removes the graph again, we're okay with that for now
               (loop for binding in bindings
                     for uri = (jsown:filter binding "resource" "value")
                     for typeObj = (jsown:val binding "type")
                     when (string= (jsown:val typeObj "type") "uri")
                       do (set-known-type uri (jsown:val typeObj "value") t)))))
         (uri-has-type (uri type)
           ;; only usable after fetching types
           (multiple-value-bind (table-for-type table-for-type-p)
               (gethash type known-type-uri-index)
             (and table-for-type-p  (gethash uri table-for-type))))
         (all-value-constraints-hold-p (quad constraint)
           "Non-nil iff all value constraints hold."
           ;; search for failing constraints and invert result
           (not
            (do-graph-constraint (constraint) (position :value value)
              (unless (detect-quads:quad-term-uri= (getf quad position) value)
                (return t)))))
         (quad-matches-constraint (quad constraint)
           (not (do-graph-constraint (constraint) (position type value)
                  (case type
                    (:value (unless (detect-quads:quad-term-uri= (getf quad position) value)
                              (return t)))
                    (:type (unless (uri-has-type (detect-quads:quad-term-uri (getf quad position))
                                                 value)
                             (return t)))
                    (otherwise
                     (format t "~&Did not understand typet ~A as constaint~%" type)
                     (return t))))))
         (move-quad (quad graph)
           (let ((new-quad (copy-seq quad)))
             (setf (getf new-quad :graph) (sparql-manipulation:iriref graph))
             new-quad))
         (mark-quad-to-store (quad)
           (push quad dispatched-quads))
         (s-p-o-is-uri-p (quad)
           ;; inverse logic for fast exiting
           (not (loop for (k v) on quad by #'cddr
                      unless (eq k :graph)
                        when (not (detect-quads:quad-term-uri v))
                          do (return t)))))
      (if (mu-auth-sudo)
          quads
          (with-access-tokens (tokens)
            ;; Initialize type index with all types mentioned in this set of quads
            (loop for quad in quads
                  for pred-string = (detect-quads:quad-term-uri (getf quad :predicate))
                  when (and pred-string
                            (s-p-o-is-uri-p quad)
                            (detect-quads:quad-term-uri= (getf quad :predicate) "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
                    do (set-known-type (detect-quads:quad-term-uri (getf quad :subject))
                                       (detect-quads:quad-term-uri (getf quad :object))
                                       t))
            ;; Find all extra knowledge we need to have on the quad's types

            ;; we need the combination of each quad and each graph constraint to
            ;; figure out what information we need
            (dolist (token-with-graph-specification (accessible-graphs-with-tokens tokens :write))
              (dolist (constraint (graph-specification-constraints (cdr token-with-graph-specification)))
                ;; find all quads for which any value constraints hold, these are hard requirements
                (loop for quad in quads
                      when (all-value-constraints-hold-p quad constraint)
                        do (do-graph-constraint (constraint) (position :type value)
                             (alexandria:when-let ((uri (detect-quads:quad-term-uri (getf quad position))))
                               ;; ask for information on the types
                               (ensure-future-type-known uri value))))))
            (fetch-types-to-fetch)
            ;; now we know we have all relevant types, we can go over the
            ;; computations and determine in which graphs each quad should be stored
            (dolist (token-with-graph-specification (accessible-graphs-with-tokens tokens :write))
              (let ((graph (token-graph-specification-graph (car token-with-graph-specification) (cdr token-with-graph-specification))))
                ;; TODO: check each quad so we only add it once
                (dolist (constraint (graph-specification-constraints (cdr token-with-graph-specification)))
                  (dolist (quad quads)
                    (when (quad-matches-constraint quad constraint)
                      (mark-quad-to-store (move-quad quad graph)))))))
            dispatched-quads)))))
