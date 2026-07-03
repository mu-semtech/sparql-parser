(in-package :odrl-config)

;; Shapes Constraint Language (SHACL)
;;
;; A, very, simplified implementation of SHACL.  This implementation is strictly limited to the
;; elements of SHACL we need in order to express which triples should be considered part of some
;; asset collection.
(defclass shape ()
  ((uri :initarg :uri)
   (target-class :initarg :target-class
                 :initform nil
                 :reader target-class)) ; sh:targetClass
  (:documentation "A SHACL shape."))

(defclass node-shape (shape)
  ((properties :initarg :properties
               :initform nil
               :reader properties) ; sh:property*
   ;; NOTE (04/09/2025): Used to indicate whether the property shapes are surrounded by a
   ;; `sh:not'. This is a simplification, ideally we can capture and process constraints
   ;; (components) in general.
   (notp :initarg :notp
         :type boolean
         :initform nil
         :reader notp))
  (:documentation "A SHACL node shape"))

(defmethod initialize-instance :after ((node node-shape) &key)
  (with-slots (target-class) node
    (unless target-class
      (error "Must supply a TARGET-CLASS for a node shape."))))

(defclass property-shape (shape)
  ((path :initarg :path
         :reader path)) ; value is a predicate URI or a `property-path' instance
  (:documentation "A SHACL property shape"))

(defmethod initialize-instance :after ((shape property-shape) &key)
  (with-slots (path) shape
    (unless path
      (error "Must supply a PATH for a property."))))

(defclass property-path ()
  ((predicate-path :initarg :predicate-path
                   :reader predicate-path)
   (object :initarg :object
           :reader object))
  (:documentation "A SHACL property path."))

(defmethod initialize-instance :after ((prop property-path) &key)
  (with-slots (predicate-path object) prop
    (unless predicate-path
      (error "Must supply a PREDICATE PATH for a property path."))
    (unless object
      (error "Must supply an OBJECT for a property path."))))

;;
;; Conversion to sparql-parser's ACL
;;
(defgeneric shacl-to-acl (shape &optional notp)
  (:documentation "Convert a SHACL shape to its corresponding sparql-parser entity."))

(defmethod shacl-to-acl ((shape node-shape) &optional notp)
  (declare (ignore notp))
  (with-slots (target-class properties notp) shape
    (alexandria:flatten
     (append
      (list (if (is-empty-node-p target-class) 'acl:_ target-class))
      (if properties
          (mapcar (lambda (prop) (shacl-to-acl prop notp)) properties)
          '(acl::-> acl:_))))))

(defun is-empty-node-p (path)
  "Check whether PATH is the special uri for an empty node.

The special uri was introduced to allow users to specify \"all predicates\" in a policy, as one
would use `_' in a lisp configuration.  This special uri was needed because in SHACL property paths
must have a value for their object and otherwise we could not express type specifications of the of
the form `TYPE <- _' or `TYPE <x _'."
  (member path '("ext:all" "http://mu.semte.ch/vocabularies/ext/all") :test #'string=))

(defun direction-string (inversep notp)
  "Determine the correct direction symbol for a predicate specification."
  (cond
    ((and inversep notp) 'acl::<x)
    ((and inversep (not notp)) 'acl::<-)
    ((and (not inversep) notp) 'acl::x>)
    (t 'acl::->)))

(defmethod shacl-to-acl ((shape property-shape) &optional notp)
  ;; If value of `path' is
  ;; - a URI: (make-... :direction "->" :predicate path)
  ;; - a `property-path':
  ;;   + parse its `predicate-path' to determine value for :direction
  ;;   + use its `object' as value for :predicate
  (with-slots (path) shape
    (list
     ;; NOTE (13/09/2025): The simplification of using the mere existence of a property path to mean
     ;; invert the direction depends on the fact that we use no other property paths than
     ;; `sh:inversePath'.  This should be generalised to actually check which `predicate-path' is
     ;; used.
     (direction-string (typep path 'property-path) notp)
     (if (typep path 'property-path)
         (if (is-empty-node-p (object path)) 'acl:_ (object path))
         (if (is-empty-node-p path) 'acl:_ path)))))

;;
;; Varia
;;
(defmethod print-object ((shape node-shape) stream)
  (print-unreadable-object (shape stream)
    (with-slots (uri target-class properties notp) shape
      (format
       stream
       "~a <~a>~&~2t<target: ~a>~&~2t<inverse: ~a>~&~2t<properties:~&~{~4t~a~&~}>"
       (type-of shape)
       uri
       target-class
       notp
       properties))))

(defmethod print-object ((shape property-shape) stream)
  (print-unreadable-object (shape stream)
    (with-slots (uri path) shape
      (format stream "~a <~a>~&~4t<path: ~a>" (type-of shape) uri path))))

(defmethod print-object ((path property-path) stream)
  (print-unreadable-object (path stream)
    (with-slots (predicate-path object) path
      (format stream "~a <~a> <~a>" (type-of path) predicate-path object))))
