(in-package :odrl-config)

;; ODRL information model
;;
;; An implementation of a simplified version of the ODRL information model.  This implementation is
;; intended to cover the parts of ODRL we currently need, and is not intended to support the entire
;; information model.  For example, this only supports Sets and Permissions, and no other types of
;; policies or rules.  Similarly, Constraints are not supported at all.
;;
;; Furthermore, this implementation explicitly deviates from ODRL's specification in some ways.
;; Consult the documentation of individual classes for more information.
(defclass concept ()
  ((uri :initarg :uri
        :reader uri))
  (:documentation "Base class for ODRL concepts."))

(defclass policy (concept)
  ((rules :initarg :rules
          :type list
          :reader rules)) ; odrl:permission
  (:documentation "An ODRL Policy consisting of a set of rules."))

(defclass rule-set (policy)
  ()
  (:documentation "An ODRL Set that represents any set of rules."))

;; TODO: Should probably replace it by something more robust.
(defun to-kebab-case (str)
  "Convert a STR to kebab case.

Note, this is a simplified version that does *not* split camel case, any upper case letters will
simply be down cased."
  (string-downcase (cl-ppcre:regex-replace-all "\\s+|_" str "-")))

;; TODO: `supply-allowed-group' allows to specify a `constraint' argument. Currently the value for
;; `constraint' will be implicitly set based on whether there is a `query' specified. Consequently,
;; it is not possible to
;; - specify `NEVER' as value for `constraint'; and
;; - overwrite the presence of a `query' by explicitly specifying `ALWAYS' (or `NEVER') as value for
;;   `constraint'.
(defclass party-collection (concept)
  ((name :initarg :name
         :reader name) ; vcard:fn
   (description :initarg :description
                :initform nil
                :reader description) ; ext:description
   (parameters :initarg :parameters
               :initform nil
               :reader parameters) ; ext:queryParameters
   (query :initarg :query
          :initform nil
          :reader query)) ; ext:definedBy
  (:documentation "An ODRL party collection.  In contrast to the ODRL specification this does not explicitly contain member parties.  Instead members are essentially defined by the query, if the query returns a result the (implied) party is considered a member of the party collection."))

(defmethod initialize-instance :after ((concept party-collection) &key)
  (setf (slot-value concept 'name) (to-kebab-case (name concept))))

;; TODO: `define-graph' allows to specify extra options `:sparql' and `:delta'. The ODRL policy
;; currently does not allow such options to be passed. Should extend data model to support this if
;; we want to achieve full compatibility with the lisp configuration interface.
(defclass asset-collection (concept)
  ((name :initarg :name
         :type string
         :reader name) ; vcard:fn
   (description :initarg :description
                :initform nil
                :reader description) ; dct:description
   (graph :initarg :graph
          :reader graph) ; ext:graphPrefix
   (assets :initarg :assets
           :type list ; of `shacl:node-shape's
           :reader assets)) ; ^odrl:partOf
  (:documentation "An ODRL Asset collection representing a graph.  In contrast to the ODRL specification this does explicitly refer to its contained assets, thereby modelling the inverse of the ODRL's partOf predicate.  This inversion simplifies converting ODRL policies to ACL configurations as it allows to iterate of the necessary assets when given an asset collection, which is in turn referenced by a rule for the starting point of the ODRL to ACL conversion.  Otherwise, one would somehow have to keep track of all asset instances and link them their collections.  A consequence of this is that the entity creating `asset-collection' instances is responsible for inverting the relations between assets and the asset collections they part of.  Furthermore, assets are represented as instances of `shacl:node-shape' and there is *no* explicit class for ODRL Assets."))

(defmethod initialize-instance :after ((concept asset-collection) &key)
  (setf (slot-value concept 'name) (to-kebab-case (name concept))))

(defclass rule (concept)
  ((actions :initarg :actions
            :type list
            :reader actions) ; odrl:action
   (target :initarg :target
           :type asset-collection
           :reader target) ; odrl:target
   (assignee :initarg :assignee
             :type party-collection
             :reader assignee)) ; odrl:assignee
  (:documentation "An ODRL rule combines the common parts for permissions, prohibitions, and duties.  In contrast to the ODRL specification we allow a rule to specify multiple actions, as `acl::access-grant's allows multiple usages to be specified."))

(defclass permission (rule)
  ()
  (:documentation "An ODRL permission represents that an assignee is allowed to perform an action on a target."))

(defclass action (concept)
  ()
  (:documentation "An ODRL Action class which indicates an operation that can be performed on an asset.  The actual operation should be encoded in the URI of the action element.  Note that the conversion to ACL currently only supports two actions: `odrl:read' and `odrl:modify', specifying any other action will lead to errors."))


;;
;; Varia
;;
(defmethod print-object ((object rule-set) stream)
  (print-unreadable-object (object stream)
    (with-slots (uri rules) object
      (format
       stream
       "~a <~a>~&~2t<permissions:~&~4t ~{~2t~a~^~&~}>"
       (type-of object)
       uri
       (mapcar #'uri rules)))))

(defmethod print-object ((object rule) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (uri object))))

(defmethod print-object ((concept action) stream)
  (print-unreadable-object (concept stream)
    (format stream "~a" (uri concept))))

(defmethod print-object ((object asset-collection) stream)
  (print-unreadable-object (object stream)
    (with-slots (uri name description graph assets) object
      (format
       stream
       "~a ~a~&~2t<name: ~a>~&~2t<description: ~a>~&~2t<graph: ~a>~&~2t<assets: ~{~&~4t~a~}>"
       (type-of object)
       uri
       name
       description
       graph
       assets))))

(defmethod print-object ((object party-collection) stream)
  (print-unreadable-object (object stream)
    (with-slots (uri name description parameters query) object
      (format
       stream
       "~a ~a~&~2t<name: ~a>~&~2t<description: ~a>~&~2t<parameters: ~a>~&~2t<query: ~a>"
       (type-of object)
       uri
       name
       description
       parameters
       query))))
