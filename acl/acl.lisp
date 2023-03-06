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
    (string-downcase (symbol-name (name access)))))

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
          (sparql-parser:parse-sparql-string (coerce (query access) 'base-string)))))

(defclass always-accessible (access)
  ()
  (:documentation "An entity which is always accessible.  Used for public resources."))

(defstruct access-token
  (access (error "Must supply ACCESS when creating ACCESS-TOKEN") :type access)
  (parameters nil :type list))

(defgeneric calculate-access-tokens (access &key mu-session-id)
  (:documentation "Yields truethy iff the given access is accessible within the current access context.")
  (:method ((access always-accessible) &key mu-session-id)
    (declare (ignore mu-session-id))
    (list (make-access-token :access access)))
  (:method ((access access-by-query) &key mu-session-id)
    (when mu-session-id ; ignore when no mu-session-id supplied
      (sparql-parser:with-sparql-ast (query access)
        (let ((query (replace-iriref (query access) :from "SESSION_ID" :to mu-session-id)))
          (unless (sparql-generator:is-valid query)
            (error "Generated invalid access query"))
          (loop for binding
                  in (-> query
                       (sparql-generator:write-valid)
                       (client:query)
                       (client:bindings))
                collect
                (make-access-token
                 :access access
                 :parameters (mapcar (lambda (var) (jsown:filter binding var "value"))
                                     (variables access)))))))))

(defun access-token-jsown (token)
  "Yields a jsown representation of the access token."
  (jsown:new-js
    ("name" (name (access-token-access token)))
    ("variables" (access-token-parameters token))))

(defun jsown-access-token (jsown)
  "Yields an ACCESS-TOKEN from the JSOWN specification."
  (make-access-token :access (find-access-by-name (jsown:val jsown "name"))
                     :parameters (jsown:val jsown "variables")))

(defstruct graph-specification
  (name (error "Must supply name to graph name") :type symbol)
  (base-graph (error "Must supply base graph string") :type string)
  (constraints nil))

(defstruct access-grant
  (usage (list :read))
  (graph-spec (error "Must supply graph spec") :type symbol)
  (access (error "Must supply which grant allows access") :type symbol))

(defparameter *active-access-rights* nil
  ;; TODO: currently not used.  Not sure if this should be globally
  ;; available as contextual information.
  "List of the current call's access rights.")

(defun access-tokens-from-allowed-groups (mu-auth-allowed-groups)
  "Calculates access rights formthe mu-auth-allowed-groups string."
  (loop for group in (jsown:parse mu-auth-allowed-groups)
        for access = (find-access-by-name (jsown:val group "name"))
        when access
          collect
          (make-access-token :access access
                             :parameters (jsown:val group "variables"))))

(defun access-tokens-from-session-id (mu-session-id)
  "Calculates the access rights from the mu-session-id."
  (loop for access in *access-specifications*
        append (calculate-access-tokens access :mu-session-id mu-session-id)))

(defun calculate-and-cache-access-tokens (mu-auth-allowed-groups mu-session-id)
  "Calculates the access rights based on the mu-auth-allowed-groups string or mu-session-id."
  (if mu-auth-allowed-groups
      (access-tokens-from-allowed-groups mu-auth-allowed-groups)
      (let ((tokens (access-tokens-from-session-id mu-session-id)))
        (setf (mu-auth-allowed-groups)
              (mapcar #'access-token-jsown tokens))
        tokens)))

(defmacro with-access-tokens ((access-tokens-var) &body body)
  "Executes BODY with ACCESS-RIGHTS-VAR bound to access rights for current
context.  Calculates access rights as needed and updates necessary
variables."
  `(let ((,access-tokens-var (calculate-and-cache-access-tokens (mu-auth-allowed-groups) (mu-session-id))))
     ,@body))

(defun graphs-for-tokens (tokens usage)
  "Yields the graphs which can be accessed from TOKENS."
  (let* ((grant-info (loop for token in tokens
                           for token-name = (name (access-token-access token))
                           append (loop for right in *rights*
                                        when (and (eq (access-grant-access right) token-name)
                                                  (find usage (access-grant-usage right) :test #'eq))
                                          append (loop for graph-specification in *graphs*
                                                       for granted-graph-spec-name = (access-grant-graph-spec right)
                                                       when (eq granted-graph-spec-name
                                                                (graph-specification-name graph-specification))
                                                         collect (cons token graph-specification))))))
    (loop for (token . graph-specification) in grant-info
          collect (format nil "~A~{~A~,^/~}"
                          (graph-specification-base-graph graph-specification)
                          (access-token-parameters token)))))

(defun apply-access-rights (query-ast &key (usage :read))
  "Applies current access rights to MATCH.

MATCH may be updated in place but updated MATCH is returned."
  (with-access-tokens (tokens)
    (-> query-ast
      (remove-dataset-clauses)
      (remove-graph-graph-patterns)
      (add-default-base-decl-to-prologue)
      (add-from-graphs (graphs-for-tokens tokens usage)))))
