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

(defclass property-shape (shape)
  ((path :initarg :path
         :initform nil
         :reader path)) ; value is a predicate URI or a `property-path' instance
  (:documentation "A SHACL property shape"))

(defclass property-path ()
  ((predicate-path :initarg :predicate-path
                   :reader predicate-path)
   (object :initarg :object
           :reader object))
  (:documentation "A SHACL property path."))
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
