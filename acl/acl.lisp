(in-package #:acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")


(defconstant _ '_ "Empty node symbolizing the default or no value.")

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

(defun access-token-equal-p (a b)
  "Returns truethy iff the two access tokens are the same."
  (and (= (length (access-token-parameters a))
          (length (access-token-parameters b)))
       (every #'string=
              (access-token-parameters a)
              (access-token-parameters b))
       (access-grant-equal-p (access-token-access a)
                             (access-token-access b))))

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

(defstruct (access-grant (:constructor make-access-grant*))
  (scopes (list '_))
  (usage (list :read))
  (graph-spec (error "Must supply graph spec") :type symbol)
  (access (error "Must supply which grant allows access") :type string))

(defun access-grant-equal-p (a b)
  "Yields truethy iff two access grants are the same."
  (flet ((equal-by (key test)
           (funcall test
                    (funcall key a)
                    (funcall key b))))
    (and (equal-by #'access-grant-access
                   #'string=)
         (equal-by #'access-grant-graph-spec
                   #'eq)
         (equal-by #'access-grant-usage
                   #'alexandria:set-equal) ; could use eq in set-equal
         (equal-by #'access-grant-scopes
                   #'alexandria:set-equal) ; could use eq in set-equal
         )))

(defun make-access-grant (&rest args &key scopes usage graph-spec access)
  "Constructs a new access grant."
  (declare (ignore usage graph-spec access))
  ;; expand the scope when it's a string
  (let ((args (copy-list args)))
    (when scopes
      (setf (getf args :scopes)
            (mapcar (lambda (scope)
                      (etypecase scope
                        (string (prefix:expand scope))
                        (symbol scope)))
                    scopes)))
    (apply #'make-access-grant* args)))

(defun access-grants-for-access-name (name)
  "Yields a list of all access grants from *rights* which have NAME as ACCESS-GRANT-ACCESS."
  (remove-if-not (lambda (grant) (string= (access-grant-access grant) name))
                 *rights*))

(defun find-access-grant-by-name (name)
  "Find access right by name."
  (find name *rights*
        :test #'string=
        :key #'access-grant-access))

(defun access-tokens-from-allowed-groups (mu-auth-allowed-groups)
  "Calculates access rights formthe mu-auth-allowed-groups string."
  (loop for group in mu-auth-allowed-groups
        for name = (jsown:val group "name")
        for parameters = (jsown:val group "variables")
        append (loop for access-grant in (access-grants-for-access-name name)
                     collect (make-access-token
                              :access access-grant
                              :parameters parameters))))

(defun access-tokens-from-session-id (mu-session-id)
  "Calculates the access rights from the mu-session-id."
  (remove-duplicates
   (loop for access in *access-specifications*
         append (calculate-access-tokens access :mu-session-id mu-session-id))
   :test #'access-token-equal-p))

(defmacro with-test-code-json-access-tokens ((json-token-string) &body body)
  `(let ((*test-code-access-tokens* (access-tokens-from-allowed-groups ,json-token-string)))
     ,@body))

(defun calculate-and-cache-access-tokens (mu-auth-allowed-groups mu-session-id mu-call-scope)
  "Calculates the access rights based on the mu-auth-allowed-groups string or mu-session-id."
  ;; The code which consumes these access tokens shouldn't _require_
  ;; only the correct access tokens to be set, they should parse it too,
  ;; yet the filtering for scope here allows us to skip some
  ;; computations.
  (remove-if-not
   (lambda (access-token)
     ;; TODO: verify systems are working with this deduplication enabled (remove t and test)
     (or t
         (find mu-call-scope (access-grant-scopes (access-token-access access-token))
               :test #'equal)))
   (if mu-auth-allowed-groups
       (access-tokens-from-allowed-groups mu-auth-allowed-groups)
       (let ((tokens (access-tokens-from-session-id mu-session-id)))
         (setf (mu-auth-allowed-groups)
               (jsown-dedup (mapcar #'access-token-jsown tokens)
                            :same-structure-p t))
         tokens))))

(defmacro with-access-tokens ((access-tokens-var) &body body)
  "Executes BODY with ACCESS-RIGHTS-VAR bound to access rights for current
context.  Calculates access rights as needed and updates necessary
variables."
  `(let ((,access-tokens-var (calculate-and-cache-access-tokens (mu-auth-allowed-groups) (mu-session-id) (mu-call-scope))))
     ,@body))

(defun accessible-graphs (&key tokens usage scope)
  "Yields a list of (CONS TOKEN GRAPH-SPECIFICATION) for the set of supplied tokens."
  (loop for token in tokens
        for access-grant = (access-token-access token)
        for token-scopes = (access-grant-scopes access-grant)
        for graph-spec = (access-grant-graph-spec access-grant)
        when (and (or (not usage)
                      (find usage (access-grant-usage access-grant) :test #'eq))
                  (find scope token-scopes :test #'equal))
          append (loop for graph in *graphs*
                       when (eq graph-spec (graph-specification-name graph))
                         collect (cons token graph))))

(defun graphs-for-tokens (tokens usage scope)
  "Yields the graphs which can be accessed from TOKENS."
  (remove-duplicates
   (loop for (token . graph-specification) in (accessible-graphs :tokens tokens :usage usage :scope scope)
         collect (token-graph-specification-graph token graph-specification))
   :test #'string=))

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
            (add-from-graphs (or (graphs-for-tokens tokens usage (mu-call-scope))
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

(defun accessible-graphs-for-current-user-and-query (usage)
  "Yields all the accessible graphs for the current user and usage."
  (with-access-tokens (tokens)
    (or (graphs-for-tokens tokens usage (mu-call-scope))
        (list "http://mu-authorization.service.semantic.works/empty-graph"))))

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
           (let ((graphs (accessible-graphs-for-current-user-and-query :read))
                 (uris (alexandria:hash-table-keys types-to-fetch)))
             (loop for (uri . types) in (type-cache::types-for uris graphs)
                   do (loop for type in types
                            do (set-known-type uri type t)))))
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
                     (format t "~&Did not understand type ~A as constaint~%" type)
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
            (dolist (token-with-graph-specification (accessible-graphs :tokens tokens :usage :write :scope (mu-call-scope)))
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
            (dolist (token-with-graph-specification (accessible-graphs :tokens tokens :usage :write :scope (mu-call-scope)))
              (let ((graph (token-graph-specification-graph (car token-with-graph-specification) (cdr token-with-graph-specification))))
                ;; TODO: check each quad so we only add it once
                (dolist (constraint (graph-specification-constraints (cdr token-with-graph-specification)))
                  (dolist (quad quads)
                    (when (quad-matches-constraint quad constraint)
                      (mark-quad-to-store (move-quad quad graph)))))))
            dispatched-quads)))))
