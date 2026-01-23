(in-package :odrl-config)

(defparameter *use-odrl-config-p* nil
  "Non-nil means the service should load its policy from a file containing an ODRL policy.")

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
;; Conversion to sparql-parser's ACL
;;
(defgeneric odrl-to-acl (concept)
  (:documentation "Convert an ODRL concept to its corresponding sparql-parser configuration macro."))

(defun rules-match-p (left right)
  "Return t if the rules LEFT and RIGHT have the same target and assignee, nil otherwise."
  (and (eq (slot-value left 'assignee) (slot-value right 'assignee))
       (eq (slot-value left 'target) (slot-value right 'target))))

(defun find-matching-rule (rule rules)
  "Find a rule in RULES that `rules-match-p' RULE."
  (find-if (lambda (r) (rules-match-p r rule)) rules))

(defun reduce-rules (rules)
  "Reduce RULES by merging together rules that have the same assignee and target."
  (let ((reduced-rules '()))
    (mapcar
     (lambda (rule)
       (let ((matching-rule (find-matching-rule rule reduced-rules)))
         (if matching-rule
             (setf (slot-value matching-rule 'actions)
                   (union (slot-value matching-rule 'actions)
                          (slot-value rule 'actions)))
             (push rule reduced-rules))))
     rules)
    reduced-rules))

(defmethod odrl-to-acl ((concept rule-set))
  (with-slots (rules) concept
    (let ((party-collections (mapcar (lambda (r) (slot-value r 'assignee)) rules))
          (asset-collections (mapcar (lambda (r) (slot-value r 'target)) rules)))
      ;; NOTE (20/01/2026): Party and Asset Collections that are not referenced by a rule are not
      ;; converted to their respective access specifications or graph specifications. Consequently,
      ;; no specifications for such collections are added the service's internal state. This differs
      ;; from the situation with a Lisp configuration where all defined specifications are
      ;; evaluated, irrelevant whether they are used in a grant.
      (handler-case
          (progn
            (mapcar #'odrl-to-acl (remove-duplicates party-collections))
            (mapcar #'odrl-to-acl (remove-duplicates asset-collections))
            ;; NOTE (24/01/2026): The `reduce-rules' merges rules that have the same assignee and
            ;; target. These mergers allow to convert each rule to a single access-grant.
            (mapcar #'odrl-to-acl (reduce-rules rules)))
        (error (e)
          (format t "~%Error: Could not parse the loaded ODRL policy: ~A~%" e))))))

(defmethod odrl-to-acl ((concept asset-collection))
  (with-slots (name graph assets) concept
    (acl::define-graph*
        :name (read-from-string name)
      :graph graph
      ;; TODO: set actual values, cf. `define-graph' macro, requires actually getting this as input
      :options '(:delta t :sparql t)
      :type-specifications (mapcar #'shacl-to-acl assets))))

(defmethod odrl-to-acl ((concept party-collection))
  (with-slots (name description parameters query) concept
    (acl:supply-allowed-group name :query query :parameters parameters)))

;; TODO: This partially replicates the logic in the `acl:grant' macro
(defmethod odrl-to-acl ((concept permission))
  (with-slots (actions target assignee) concept
    (acl:grant*
     :scopes (list 'acl:_) ;; TODO: support scopes
     :rights (mapcar
              (lambda (action)
                (intern (symbol-name (odrl-to-acl action)) :keyword))
              actions)
     :graph-specs (list (read-from-string (slot-value target 'name)))
     :allowed-groups (list (slot-value assignee 'name)))))

(defmethod odrl-to-acl ((concept action))
  (with-slots (uri) concept
    (cond
      ((cl-ppcre:scan ".*read>?$" uri) 'acl::read)
      ((cl-ppcre:scan ".*modify>?$" uri) 'acl::write)
      ;; NOTE (23/01/2026): The odrl:write action was deprecated by odrl:modify. We will support it
      ;; anyway for convenience.
      ((cl-ppcre:scan ".*write>?$" uri) 'acl::write)
      (t (error "No matching right found for \"~a\"" uri)))))

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
