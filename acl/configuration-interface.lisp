(in-package :acl)

;;;;;;;;;;;;;;;;;;;;;;
;;; what is in a graph

(defun add-or-replace-graph (graph-specification)
  "Adds a graph specification or replaces an earlier one with the same name."
  (let ((name (graph-specification-name graph-specification)))
    (when (find name *graphs* :key #'graph-specification-name)
      (warn "Replacing earlier graph specification for ~A" name)
      (setf *graphs* (delete name *graphs* :key #'graph-specification-name)))
    (push graph-specification *graphs*)))

(defun define-graph* (&key name graph type-specifications specifications-list)
  (add-or-replace-graph
   (make-graph-specification
    :name name
    :base-graph graph
    :constraints (concatenate
                  'list
                  (loop for (type-name . predicate-specifications) in type-specifications
                        for group = (gensym "GROUP")
                        for type-sub-constraint = (if (eq type-name '_) nil `(:type ,(prefix:expand type-name)))
                        if predicate-specifications
                          append (loop for (direction predicate) on predicate-specifications
                                         by #'cddr
                                       for type-constraint
                                         = (when type-sub-constraint
                                             (case direction
                                               (-> `(:subject ,type-sub-constraint))
                                               (<- `(:object ,type-sub-constraint))
                                               (x> `(:subject ,type-sub-constraint))
                                               (<x `(:object ,type-sub-constraint))
                                               (otherwise (error "Direction must be <- or -> but got ~s" direction))))
                                       for predicate-constraint
                                         = (cond ((eq predicate '_) nil)
                                                 ((find direction '(-> <-))
                                                  `(:predicate (:value ,(prefix:expand predicate))))
                                                 ((find direction '(x> <x))
                                                  `(:predicate (:not-value ,(prefix:expand predicate))
                                                    :group ,group)))
                                       collect `(,@type-constraint ,@predicate-constraint))
                        else
                          ;; shorthand for all predicates
                          collect (if type-name `(:subject ,type-sub-constraint) `()))
                  specifications-list))))

(defmacro define-graph (name (graph) &body type-specifications)
  "Compact DSL for specifying common graph constraints."
  `(define-graph*
    :name ',name
    :graph ,graph
    :type-specifications ',type-specifications))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; when can we read-write what for our allowed groups?

(defmacro with-scope (scope &body body)
  `(let ((current-scope ,scope))
     (declare (special current-scope))
     ,@body))

(defun grant* (&key scopes graph-specs rights allowed-groups)
  "Functional variant to apply a grant."
  (dolist (allowed-group allowed-groups)
    (dolist (graph-spec graph-specs)
      (push (make-access-grant
             :usage rights
             :graph-spec graph-spec
             :scopes (loop for scope in scopes
                           if (eq scope '_)
                             collect scope
                           else
                             collect (prefix:expand scope))
             :access allowed-group)
            *rights*))))

(declaim (special current-scope))

(defmacro grant (right &key to-graph for-allowed-group to for scopes)
  (flet ((ensure-list (thing)
           (if (listp thing)
               `(,@thing)
               `(,thing))))
    `(grant* :scopes (cond
                       (,scopes ,scopes)
                       ((boundp 'current-scope)
                        (list current-scope))
                       (t (list '_)))
             :graph-specs ',(append (ensure-list to-graph) (ensure-list to))
             :rights (list ,@(loop for item in (if (listp right) right (list right))
                                   collect (intern (symbol-name item) :keyword)))
             :allowed-groups ',(append (ensure-list for-allowed-group) (ensure-list for)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calculating the mu-auth-allowed-groups

(defun supply-allowed-groups* (allowed-group constraint &rest args &key query parameters &allow-other-keys)
  "Indicates when an allowed-group should be supplied.

Understands two constraints natively:
- NIL :: same as always
- ALWAYS :: always supply this group, no rules necessary.
- NEVER :: never supply this group, may not define group.
- QUERY :: construct access-by-query with supplied query and parameters

In case constraint is not understood, make-instance is called with constraint :name allowed-groups args"
  (declare (ignore parameters))
  (unless constraint
    (if query
        (setf constraint 'query)
        (setf constraint 'always)))

  (let ((instance (case constraint
                    (always
                     (make-instance 'always-accessible
                                    :name allowed-group))
                    (never nil)
                    (query
                     (make-instance 'access-by-query
                                    :name allowed-group
                                    :query (or (getf args :query)
                                               (error "Must supply query when constructing allowed group by query for group ~A"
                                                      allowed-group))
                                    :vars (getf args :parameters)))
                    (otherwise (apply #'make-instance
                                      constraint
                                      args)))))
    (when instance
      (push instance *access-specifications*))))

(defmacro supply-allowed-group (group &body args &key constraint parameters query &allow-other-keys)
  (declare (ignore query))
  (let ((args (copy-list args)))
    ;; quote arguments when they're lists
    (when (and parameters
               (listp parameters)
               (every #'stringp parameters))
      (setf (getf args :parameters)
            `(list ,@parameters)))
    (when (and constraint (not (listp constraint)))
      (setf constraint `',constraint))
    `(supply-allowed-groups* ,group
                             ,constraint
                             ,@args)))
