(in-package #:acl)

(defclass access ()
  ()
  (:documentation "Specifies how a thing can be accessed."))

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
          (sparql-parser:parse-sparql-string (query access)))))

(defclass always-accessible (access)
  ()
  (:documentation "An entity which is always accessible.  Used for public resources."))

(defgeneric is-accessible (access)
  (:documentation "Yields truethy iff the given access is accessible within the current access context.")
  (:method ((access always-accessible))
    (declare (ignore access))
    t)
  (:method ((access access-by-query))
    (when (mu-session-id) ; ignore when no mu-session-id supplied
      (let ((query (sparql-manipulation:replace-iriref (query access)
                                                       :from "SESSION_ID"
                                                       :to mu-session-id)))
        ))
    (error "Implementation of is-accessible has not been implemented for
access-by-query yet.")))

(defstruct graph
  (graph "http://mu.semte.ch/application")
  (usage (list :read))
  (scope nil))

(defstruct access-right
  (access (error "Must supply access when creating access right") :type access)
  (graphs (error "Must supply graphs when creating access right") :type graph))

(defparameter *active-access-rights* nil
  ;; TODO: currently not used.  Not sure if this should be globally
  ;; available as contextual information.
  "List of the current call's access rights.")

(defun access-tokens-from-allowed-groups (mu-auth-allowed-groups)
  "Calculates access rights formthe mu-auth-allowed-groups string."
  (jsown:parse mu-auth-allowed-groups))

(defun access-tokens-from-session-id (mu-session-id)
  "Calculates the access rights from the mu-session-id."
  (declare (ignore mu-session-id))
  (error "Calculating access rights from the mu-session-id is not yet supported."))

(defun calculate-and-cache-access-tokens (mu-auth-allowed-groups mu-session-id)
  "Calculates the access rights based on the mu-auth-allowed-groups string or mu-session-id."
  (if mu-auth-allowed-groups
      (access-tokens-from-allowed-groups mu-auth-allowed-groups)
      (let ((tokens (access-tokens-from-session-id mu-session-id)))
        (setf (mu-auth-allowed-groups)
              (jsown:to-json tokens))
        tokens)))

(defun filter-access-rights-for-scope (rights scope)
  "Filters the supplied set of access rights for the given scope."
  (loop for right in rights
        when (access-right-has-scope scope)
          collect right))

(defmacro with-access-tokens ((access-tokens-var) &body body)
  "Executes BODY with ACCESS-RIGHTS-VAR bound to access rights for current
context.  Calculates access rights as needed and updates necessary
variables."
  `(let ((,access-tokens-var (calculate-and-cache-access-tokens (mu-auth-allowed-groups) (mu-session-id))))
     ,@body))

(defmacro with-access-rights ((access-rights-var) &body body)
  (let ((access-tokens-sym (gensym "TOKENS")))
    `(with-acess-tokens (,access-tokens-sym)
       ())))

(defun apply-access-rights (query-ast)
  "Applies current access rights to MATCH.

MATCH may be updated in place but updated MATCH is returned."
    ;; (-> match
    ;;   (remove-dataset-clauses)
    ;;   (remove-graph-graph-patterns)
    ;;   (add-from-graphs (list "<http://mu.semte.ch/graphs/public>")))
  (with-access-tokens (tokens)
    (-> query-ast
      (remove-dataset-clauses)
      (remove-graph-graph-patterns)
      (add-from-graphs (list "http://mu.semte.ch/graphs/public")))))
