(in-package :odrl-config)

;; Read policy files
;;
;; Read an ODRL policy specified as n-triples in a file and convert the parsed triples to a
;; `rule-set'.
;; NOTE (23/01/2026): This is rather messy code, but once we can directly read and parse ttl files
;; at part of it should become unnecessary. The need to read and parse n-triples is due to a lack of
;; a ttl parser in common lisp. We plan to fill that gap in the future.

(defun policy-file (&optional filename)
  "Get the path to the file to read the ODRL policy from.

If FILENAME is nil, fall back to the \"config\" as default name for the policy file."
  (if (find :docker *features*)
      (concatenate 'string "../config/" (or filename "config") ".nt")
      "test/example-config.nt"))

(defun read-ntriples-file (path)
  "Read the n-triples file `policy-file' and return its contents as a single string."
  (let ((path (asdf:system-relative-pathname :sparql-parser path)))
    (alexandria:read-file-into-string path)))

(defun load-policy-file (&optional filename)
  "Read the ODRL policy from FILENAME."
  (handler-case
      (let ((path (policy-file filename)))
        (format t "~& >> INFO: Reading ODRL policy from ~A" path)
        (nt:parse-nt (read-ntriples-file path)))
    (error (e)
      (format t "~& >> WARN: An error occurred when trying to read the configuration file: ~% >>>> '~A'~%" e))))

;; Utilities
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
  "Return a string containing the full uri for the predicate matching INDICATOR."
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
  "Return a string containing the full uri for the resource type matching INDICATOR."
  (getf resource-types-plist indicator))

;;
;; Utilities to simplify parsing triples
;;
(defun value-from-object (object)
  "Return the value of the OBJECT as a string.

OBJECT is assumed to be an object as created by cl-ntriples. For any other kind of input the result
will be NIL."
  (or (getf object :object-uriref)
      (getf object :object-node-id)
      (getf object :literal-string)))

(defun triple-subject (triple)
  "Return the subject of TRIPLE as a string."
  (first triple))

(defun triple-predicate (triple)
  "Return the predicate of TRIPLE as a string."
  (second triple))

(defun triple-object (triple)
  "Return the cl-ntriples object that is the object of TRIPLE."
  (third triple))

(defun triple-object-value (triple)
  "Return the value of the TRIPLE's object as a string."
  (value-from-object (triple-object triple)))

(defun triples-for-predicate (predicate triples)
  "Return all elements in TRIPLES that have PREDICATE as predicate value."
  (remove-if-not
   (lambda (triple) (string= predicate (triple-predicate triple)))
   triples))

(defun triples-for-resource (resource triples)
  "Return an triple objects in TRIPLES that have RESOURCE as subject."
  (remove-if-not
   (lambda (triple) (string= resource (triple-subject triple)))
   triples))

(defun triples-for-resource-predicate (resource predicate triples)
  "Return all triple objects in TRIPLES that have RESOURCE as subject and PREDICATE as predicate."
  (remove-if-not
   (lambda (triple)
     (and (string= resource (triple-subject triple))
          (string= predicate (triple-predicate triple))))
   triples))

(defun triples-for-predicate-object (predicate object triples)
  "Return all triple objects in TRIPLES that have PREDICATE as predicate and OBJECT as object."
  (remove-if-not
   (lambda (triple)
     (and (string= predicate (triple-predicate triple))
          (string= object (triple-object-value triple))))
   triples))

(defun list-parts-in-collection (uri triples)
  "Return a list of the uris of all resources that are a part of the collection resource URI in TRIPLES."
  (let ((parts (triples-for-predicate-object (predicate-uri :odrl-part-of) uri triples)))
    (mapcar #'triple-subject parts)))

(defun filter-resources-for-type (type triples)
  "Filter the type triples for resources of TYPE in TRIPLES.

TYPE should be a string containing a uri for a resource type."
  (remove-if-not
   (lambda (triple) (string= type (triple-object-value triple)))
   (triples-for-predicate (predicate-uri :rdf-type) triples)))

(defun list-resource-uris (type triples)
  "Return a list containing the uri of each resource of TYPE in TRIPLES."
  (mapcar #'triple-subject (filter-resources-for-type type triples)))

(defun list-party-collections (triples)
  "List the uris for ODRL party collection resources in TRIPLES."
  (list-resource-uris (type-uri :odrl-party-collection) triples))

(defun list-asset-collections (triples)
  "List the uris for ODRL asset collection resources in TRIPLES."
  (list-resource-uris (type-uri :odrl-asset-collection) triples))

(defun list-assets (triples)
  "List the uris for ODRL asset resources in TRIPLES."
  (list-resource-uris (type-uri :odrl-asset) triples))

(defun list-permissions-in-policy (triples)
  "Return a list of the uris of all permissions in the policy defined by TRIPLES."
  (mapcar
   (lambda (triple) (triple-object-value triple))
   (triples-for-predicate (predicate-uri :odrl-permission) triples)))

;; NOTE (01/10/2025): These macros are use to make the init-forms in the `let' operators in the
;; conversion functions more readable.
(defmacro first-value-for-predicate (predicate triples)
  "Return the value of the first object for PREDICATE encountered in TRIPLES."
  `(triple-object-value (car (triples-for-predicate ,predicate ,triples))))

(defmacro first-triple-for-resource (uri triples)
  "Return the first triple with URI as subject in TRIPLES."
  `(car (triples-for-resource ,uri ,triples)))

(defun find-policy-uri (triples)
  "Find the uri for the policy resource defined in TRIPLES."
  (car (list-resource-uris (type-uri :odrl-set) triples)))

;;
;; Conversion to ODRL
;;
(defun find-concept-with-uri (uri concepts)
  "Find the concept instance in CONCEPTS that has URI as value for its uri slot."
  (when uri
    (find-if
     (lambda (concept) (string= (slot-value concept 'uri) uri))
     concepts)))

(defun find-shape-with-uri (uri shapes)
  "Find the shape instance in SHAPES that has URI as value for its uri slot."
  (when uri
    (find-if
     (lambda (shape) (string= (slot-value shape 'uri) uri))
     shapes)))

(defun make-rule-set (triples)
  "Make an `rule-set' instance for the resource with URI."
  (let ((asset-collections (make-asset-collections triples))
        (party-collections (make-party-collections triples))
        (permissions (list-permissions-in-policy triples)))
    (make-instance
     'rule-set
     :uri (find-policy-uri triples)
     :rules (mapcar
             (lambda (permission)
               (make-permission permission asset-collections party-collections triples))
             permissions))))

(defun make-party-collections (triples)
  "Make an `party-collection' for each party collection resource in TRIPLES."
  (mapcar
   (lambda (uri) (make-party-collection uri triples))
   (list-party-collections triples)))

(defun make-party-collection (uri policy-triples)
  "Make an `party-collection' instance for the resource with URI."
  (let* ((triples (triples-for-resource uri policy-triples))
         (name (first-value-for-predicate (predicate-uri :vcard-fn) triples))
         (description (first-value-for-predicate (predicate-uri :dcterms-description) triples))
         (parameters (triples-for-predicate (predicate-uri :ext-query-parameters) triples))
         (query (first-value-for-predicate (predicate-uri :ext-defined-by) triples)))
    (make-instance
     'party-collection
     :uri uri
     :name name
     :description description
     :parameters (mapcar #'triple-object-value parameters)
     ;; TODO: Make sure to remove any newlines and/or trailing spaces at the end of the string;
     ;; otherwise it will not be parsed correctly
     ;; Also remove any newlines at the beginning of the string
     :query query)))

(defun make-asset-collections (triples)
  "Make an `asset-collection' for each asset collection resource in TRIPLES."
  (let ((assets (make-node-shapes triples)))
    (mapcar
     (lambda (uri) (make-asset-collection uri assets triples))
     (list-asset-collections triples))))

(defun make-asset-collection (uri assets policy-triples)
  "Make an `asset-collection' instance for the resource with URI."
  (let* ((triples (triples-for-resource uri policy-triples))
         (name (first-value-for-predicate (predicate-uri :vcard-fn) triples))
         (description (first-value-for-predicate (predicate-uri :dcterms-description) triples))
         (graph (first-value-for-predicate (predicate-uri :ext-graph-prefix) triples))
         (assets-in-collection (list-parts-in-collection uri policy-triples)))
    (make-instance
     'asset-collection
     :uri uri
     :name name
     :description description
     :graph graph
     :assets (mapcar
              (lambda (uri) (find-shape-with-uri uri assets))
              assets-in-collection))))

(defun make-node-shapes (triples)
  "Make a `node-shape' instance for each ODRL asset resource in triples."
  (mapcar
   (lambda (uri) (make-node-shape uri triples))
   (list-assets triples)))

(defun make-node-shape (uri policy-triples)
  "Make a `shacl:node-shape' for the resource with URI."
  (let* ((triples (triples-for-resource uri policy-triples))
         (target (first-value-for-predicate (predicate-uri :sh-target-class) triples))
         ;; NOTE (01/10/2025): Node shapes may surround their property shapes with a "sh:not"
         ;; constraint component.  The `not-triple' will have a non-nil value if that is the case,
         ;; otherwise it will be nill.  This is used in `properties' to determine whether one has to
         ;; go passed an additional blank node or not to find the properties in a node shape.
         (not-triple (car (triples-for-predicate (predicate-uri :sh-not) triples)))
         (properties (if not-triple
                         (triples-for-resource-predicate
                          (triple-object-value not-triple)
                          (predicate-uri :sh-property)
                          policy-triples)
                         (triples-for-predicate (predicate-uri :sh-property) triples))))
    (make-instance
     'node-shape
     :uri uri
     :target-class target
     :properties (mapcar
                  (lambda (uri) (make-property-shape uri policy-triples))
                  (mapcar #'triple-object-value properties))
     :notp (when not-triple t))))

(defun blank-node-uri-p (uri)
  "Check whether a given URI is for a blank."
  ;; TODO(C): match on alphanumeric characters in id part
  (cl-ppcre:scan "<?http://lblod.data.gift/bnode/.+>?" uri))

(defun make-property-shape (uri policy-triples)
  "Make a `shacl:property-shape' instance for the resource with URI."
  (let ((path (triple-object-value (first-triple-for-resource uri policy-triples))))
    (make-instance
     'property-shape
     :uri uri
     :path (if (blank-node-uri-p path)
               (make-property-path path policy-triples)
               path))))

(defun make-property-path (uri policy-triples)
  "Make a `property-path' instance for the resource with URI."
  (let* ((triple (first-triple-for-resource uri policy-triples))
         (path (triple-predicate triple))
         (object (triple-object-value triple)))
    (make-instance 'property-path :predicate-path path :object object)))

(defun make-permission (uri asset-col party-col policy-triples)
  "Make a `permission' instance for the resource with URI.

ASSET-COL and PARTY-COL should be lists of, respectively, `asset-collection' and
`party-collection' instances with which the created `permission' instance can be linked."
  (let* ((triples (triples-for-resource uri policy-triples))
         (action (first-value-for-predicate (predicate-uri :odrl-action) triples))
         (target (find-concept-with-uri
                  (first-value-for-predicate (predicate-uri :odrl-target) triples)
                  asset-col))
         (assignee (find-concept-with-uri
                    (first-value-for-predicate (predicate-uri :odrl-assignee) triples)
                    party-col)))
    (make-instance
     'permission
     :uri uri
     :actions (list (make-action action))
     :target target
     :assignee assignee)))

(defun make-action (uri)
  "Make an `action' instance for the given URI."
  (make-instance 'action :uri uri))
