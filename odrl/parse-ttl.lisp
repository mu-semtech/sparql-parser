(in-package :odrl-config)

(defun policy-file (&optional filename)
  "Get the path to the file to read the ODRL policy from.

If FILENAME is nil, fall back to \"config\" as default filename for the policy file."
  (if (find :docker *features*)
      (concatenate 'string "../config/" (or filename "config") ".ttl")
      "test/example-config.ttl"))

(defun read-policy-file (path)
  "Read the policy file at PATH and return its contents as a single string."
  (let ((path (asdf:system-relative-pathname :sparql-parser path)))
    (alexandria:read-file-into-string path)))

(defun load-policy-file (&optional filename)
  "Read the ODRL policy from FILENAME."
  (handler-case
      (let ((path (policy-file filename)))
        (format t "~& >> INFO: Reading ODRL policy from ~A" path)
        (cl-ttl-parser:parse-ttl (read-policy-file path)))
    (error (e)
      (format t "~& >> WARN: An error occurred when trying to read the configuration file: ~% >>>> '~A'~%" e))))

;;
;; Utilities to process policy graph
;;
(defparameter predicates-plist
  '(:dcterms-description "http://purl.org/dc/terms/description"
    :ext-defined-by "http://mu.semte.ch/vocabularies/ext/definedBy"
    :ext-graph-prefix "http://mu.semte.ch/vocabularies/ext/graphPrefix"
    :ext-query-parameters "http://mu.semte.ch/vocabularies/ext/queryParameters"
    :odrl-action "http://www.w3.org/ns/odrl/2/action"
    :odrl-assignee "http://www.w3.org/ns/odrl/2/assignee"
    :odrl-assigner "http://www.w3.org/ns/odrl/2/assigner"
    :odrl-part-of "http://www.w3.org/ns/odrl/2/partOf"
    :odrl-permission "http://www.w3.org/ns/odrl/2/permission"
    :odrl-profile "http://www.w3.org/ns/odrl/2/profile"
    :odrl-target "http://www.w3.org/ns/odrl/2/target"
    :rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    :sh-inverse-path "http://www.w3.org/ns/shacl#inversePath"
    :sh-not "http://www.w3.org/ns/shacl#not"
    :sh-path "http://www.w3.org/ns/shacl#path"
    :sh-property "http://www.w3.org/ns/shacl#property"
    :sh-target-class "http://www.w3.org/ns/shacl#targetClass"
    :vcard-fn "http://www.w3.org/2006/vcard/ns#fn")
  "A plist containing the full uris for the predicates that are used in ODRL policies.")

(defun predicate-uri (indicator)
  "Return the full uri for the predicate matching INDICATOR as a string."
  (getf predicates-plist indicator))

(defparameter resource-types-plist
  '(:odrl-asset "http://www.w3.org/ns/odrl/2/Asset"
    :odrl-asset-collection "http://www.w3.org/ns/odrl/2/AssetCollection"
    :odrl-party "http://www.w3.org/ns/odrl/2/Party"
    :odrl-party-collection "http://www.w3.org/ns/odrl/2/PartyCollection"
    :odrl-permission "http://www.w3.org/ns/odrl/2/Permission"
    :odrl-profile "http://www.w3.org/ns/odrl/2/Profile"
    :odrl-set "http://www.w3.org/ns/odrl/2/Set"
    :sh-node-shape "http://www.w3.org/ns/shacl#NodeShape"
    :sh-property-shape "http://www.w3.org/ns/shacl#PropertyShape")
  "A plist containing the full uris for the resources types used in ODRL policies.")

(defun type-uri (indicator)
  "Return the full uri for the resource type matching INDICATOR as a string."
  (getf resource-types-plist indicator))

(defun triple-subject (triple)
  "Return the subject of TRIPLE as a string."
  (first triple))

(defun triple-predicate (triple)
  "Return the predicate of TRIPLE as a string."
  (second triple))

(defun triple-object (triple)
  "Return the object of TRIPLE."
  (third triple))

(defun uri-string (uri)
  "Return the string representing URI."
  (if (quri:uri-p uri)
      (quri:render-uri uri)
      uri))

(defun uri-equal-p (left right)
  "Check whether LEFT and RIGHT are either identical uris or blank node labels."
  (cond
    ((and (quri:uri-p left) (quri:uri-p right)) (quri:uri-equal left right))
    ((and (stringp left) (stringp right)) (string= left right))
    ((quri:uri-p left) (string= (quri:render-uri left) right))
    ((quri:uri-p right) (string= left (quri:render-uri right)))
    (t nil)))

(defun filter-subject (resource graph)
  "Keep only triples in GRAPH that have RESOURCE as subject."
  (remove-if-not
   (lambda (triple) (uri-equal-p resource (triple-subject triple)))
   graph))

(defun filter-predicate (predicate graph)
  "Return all triples in GRAPH that have PREDICATE as predicate value."
  (remove-if-not
   (lambda (triple) (uri-equal-p predicate (triple-predicate triple)))
   graph))

(defun filter-subject-predicate (resource predicate graph)
  "Return all triples in GRAPH that have RESOURCE as subject and PREDICATE as predicate."
  (remove-if-not
   (lambda (triple)
     (and (uri-equal-p resource (triple-subject triple))
          (uri-equal-p predicate (triple-predicate triple))))
   graph))

(defun filter-predicate-object (predicate object graph)
  "Return all triple objects in GRAPH that have PREDICATE as predicate and OBJECT as object."
  (remove-if-not
   (lambda (triple)
     (and (uri-equal-p predicate (triple-predicate triple))
          (or (uri-equal-p object (triple-object triple))
              (equal object (triple-object triple)))))
   graph))

(defun list-parts-of-collection (uri graph)
  "Return a list of the uris of all resources that are a part of the collection resource URI in GRAPH."
  (let ((parts (filter-predicate-object (predicate-uri :odrl-part-of) uri graph)))
    (mapcar #'triple-subject parts)))

(defun filter-resources-for-type (type graph)
  "Filter the type triples for resources of TYPE in GRAPH."
  (remove-if-not
   (lambda (triple) (uri-equal-p type (triple-object triple)))
   (filter-predicate (predicate-uri :rdf-type) graph)))

(defun list-resource-uris (type graph)
  "Return a list containing the uri of each resource of TYPE in GRAPH."
  (mapcar #'triple-subject (filter-resources-for-type type graph)))

(defun list-assets (graph)
  "List the uris for ODRL asset resources in GRAPH."
  (list-resource-uris (type-uri :odrl-asset) graph))

(defun list-permissions-in-policy (graph)
  "Return a list of the uris of all permissions in the policy defined by GRAPH."
  (mapcar
   (lambda (triple) (triple-object triple))
   (filter-predicate (predicate-uri :odrl-permission) graph)))

(defun list-asset-collections (graph)
  "List the uris for ODRL asset collection resources in GRAPH."
  (list-resource-uris (type-uri :odrl-asset-collection) graph))

(defun list-party-collections (graph)
  "List the uris for ODRL party collection resources in GRAPH."
  (list-resource-uris (type-uri :odrl-party-collection) graph))

(defun find-policy-uri (graph)
  "Find the uri for the policy resource defined in GRAPH."
  (car (list-resource-uris (type-uri :odrl-set) graph)))

;; NOTE (01/10/2025): These macros are used to make the init-forms in the `let' operators in the
;; conversion functions more readable.
(defmacro first-value-for-predicate (predicate graph)
  "Return the value of the first object for PREDICATE encountered in GRAPH."
  `(triple-object (car (filter-predicate ,predicate ,graph))))

(defmacro first-triple-for-resource (uri graph)
  "Return the first triple with URI as subject in GRAPH."
  `(car (filter-subject ,uri ,graph)))

;;
;; Conversion to ODRL
;;
(defun find-shape-with-uri (uri shapes)
  "Find the shape instance in SHAPES that has URI as value for its uri slot."
  (when uri
    (find-if
     (lambda (shape) (uri-equal-p (slot-value shape 'uri) uri))
     shapes)))

(defun find-concept-with-uri (uri concepts)
  "Find the concept instance in CONCEPTS that has URI as value for its uri slot."
  (when uri
    (find-if
     (lambda (concept) (uri-equal-p (slot-value concept 'uri) uri))
     concepts)))

(defun make-rule-set (graph)
  "Make a `rule-set' instance for the policy described by GRAPH."
  (let ((asset-collections (make-asset-collections graph))
        (party-collections (make-party-collections graph))
        (permissions (list-permissions-in-policy graph)))
    (make-instance
     'rule-set
     :uri (find-policy-uri graph)
     :rules (mapcar
             (lambda (permission)
               (make-permission permission asset-collections party-collections graph))
             permissions))))

;; Party collections
(defun make-party-collections (graph)
  "Make an `party-collection' for each party collection resource in GRAPH."
  (mapcar
   (lambda (uri) (make-party-collection uri graph))
   (list-party-collections graph)))

(defun make-party-collection (uri graph)
  "Make a `party-collection' instance for the resource with URI."
  (let* ((triples (filter-subject uri graph))
         (name (first-value-for-predicate (predicate-uri :vcard-fn) triples))
         (description (first-value-for-predicate (predicate-uri :dcterms-description) triples))
         (parameters (filter-predicate (predicate-uri :ext-query-parameters) triples))
         (query (first-value-for-predicate (predicate-uri :ext-defined-by) triples)))
    (make-instance
     'party-collection
     :uri (uri-string uri)
     :name (cl-ttl-parser:rdf-literal-value name)
     :description (when description (cl-ttl-parser:rdf-literal-value description))
     :parameters (mapcar
                  (lambda (triple)
                    (cl-ttl-parser:rdf-literal-value (triple-object triple)))
                  parameters)
     ;; TODO: Make sure to remove any newlines and/or trailing spaces at the end of the string;
     ;; otherwise it will not be parsed correctly
     ;; Also remove any newlines at the beginning of the string
     :query (when query (cl-ttl-parser:rdf-literal-value query)))))

;; Asset Collections and Assets (Node shapes)
(defun make-asset-collections (graph)
  "Make an `asset-collection' for each asset collection resource in GRAPH."
  (let ((assets (make-node-shapes graph)))
    (mapcar
     (lambda (uri) (make-asset-collection uri assets graph))
     (list-asset-collections graph))))

(defun make-asset-collection (uri assets graph)
  "Make an `asset-collection' instance for the resource with URI."
  (let* ((triples (filter-subject uri graph))
         (name (first-value-for-predicate (predicate-uri :vcard-fn) triples))
         (description (first-value-for-predicate (predicate-uri :dcterms-description) triples))
         (graph-uri (first-value-for-predicate (predicate-uri :ext-graph-prefix) triples))
         (assets-in-collection (list-parts-of-collection uri graph)))
    (make-instance
     'asset-collection
     :uri (uri-string uri)
     :name (cl-ttl-parser:rdf-literal-value name)
     :description (when description (cl-ttl-parser:rdf-literal-value description))
     :graph (uri-string graph-uri)
     :assets (mapcar
              (lambda (uri) (find-shape-with-uri uri assets))
              assets-in-collection))))

(defun make-node-shapes (graph)
  "Make a `node-shape' instance for each ODRL asset resource in graph."
  (mapcar
   (lambda (uri) (make-node-shape uri graph))
   (list-assets graph)))

(defun make-node-shape (uri graph)
  "Make a `node-shape' for the resource with URI."
  (let* ((triples (filter-subject uri graph))
         (target (first-value-for-predicate (predicate-uri :sh-target-class) triples))
         ;; NOTE (01/10/2025): Node shapes may surround their property shapes with a "sh:not"
         ;; constraint component.  The `not-triple' will have a non-nil value if that is the case,
         ;; otherwise it will be nill.  This is used in `properties' to determine whether one has to
         ;; go passed an additional blank node or not to find the properties in a node shape.
         (not-triple (car (filter-predicate (predicate-uri :sh-not) triples)))
         (properties (if not-triple
                         (filter-subject-predicate
                          (triple-object not-triple)
                          (predicate-uri :sh-property)
                          graph)
                         (filter-predicate (predicate-uri :sh-property) triples))))
    (make-instance
     'node-shape
     :uri (uri-string uri)
     :target-class (uri-string target)
     :properties (mapcar
                  (lambda (prop) (make-property-shape prop graph))
                  (mapcar #'triple-object properties))
     :notp (when not-triple t))))

(defun make-property-shape (uri graph)
  "Make a `property-shape' instance for the resource with URI."
  (let ((path (triple-object (first-triple-for-resource uri graph))))
    (make-instance
     'property-shape
     :uri (uri-string uri)
     :path (if (quri:uri-p path)
               (uri-string path)
               (make-property-path path graph)))))

(defun make-property-path (uri graph)
  "Make a `property-path' instance for the resource with URI."
  (let* ((triple (first-triple-for-resource uri graph))
         (path (triple-predicate triple))
         (object (triple-object triple)))
    (make-instance
     'property-path
     :predicate-path (uri-string path)
     :object (uri-string object))))

(defun make-permission (uri asset-col party-col graph)
  "Make a `permission' instance for the resource with URI.

ASSET-COL and PARTY-COL should be lists of, respectively, `asset-collection' and
`party-collection' instances with which the created `permission' instance can be linked."
  (let* ((triples (filter-subject uri graph))
         (action (first-value-for-predicate (predicate-uri :odrl-action) triples))
         (target (find-concept-with-uri
                  (first-value-for-predicate (predicate-uri :odrl-target) triples)
                  asset-col))
         (assignee (find-concept-with-uri
                    (first-value-for-predicate (predicate-uri :odrl-assignee) triples)
                    party-col)))
    (make-instance
     'permission
     :uri (uri-string uri)
     :actions (list (make-action action))
     :target target
     :assignee assignee)))

(defun make-action (uri)
  "Make an `action' instance for the given URI."
  (make-instance 'action :uri (uri-string uri)))
