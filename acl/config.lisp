(in-package :acl)

(defun push-or-replace* (object list set-function &key (test #'eq) (key #'identity) on-overwrite)
  "Non-macro form to add an object to place or replaces an earlier one with the same name."
  ;; ensure the item is removed
  (let ((object-key (funcall key object)))
    (when (find object-key list :key key :test test)
      (when on-overwrite (funcall on-overwrite))
      (setf list (delete object list :test test :key key))))
  ;; with the previous item removed, we can set the function
  (funcall set-function (cons object list)))

(defmacro push-or-replace (object place &rest args &key test key on-overwrite)
  "Adds an object to place or replaces an earlier one with the same name."
  (declare (ignore test key on-overwrite))
  (let ((updated-args (loop for (key val) on args by #'cddr
                            if (eq key :on-overwrite)
                              append `(,key (lambda () ,val))
                            else
                              append (list key val))))
    `(push-or-replace* ,object ,PLACE
                       (lambda (x) (setf ,place x))
                       ,@updated-args)))

(defun add-or-replace-graph (graph-specification)
  "Adds a graph specification or replaces an earlier one with the same name."
  (let ((name (graph-specification-name graph-specification)))
    (when (find name *graphs* :key #'graph-specification-name)
      (warn "Replacing earlier graph specification for ~A" name)
      (setf *graphs* (delete name *graphs* :key #'graph-specification-name)))
    (push graph-specification *graphs*))
  ;; (push-or-replace graph-specification *graphs*
  ;;                  :key #'graph-specification-name
  ;;                  :on-overwrite (warn "Replacing earlier key of graph-specification ~A"
  ;;                                      (graph-specification-name graph-specification)))
  )

;; (defmacro define-graph (name (graph) &body type-specifications)
;;   "Compact DSL for specifying common graph constraints."
;;   `(add-or-replace-graph
;;     (make-graph-specification
;;      :name ',name
;;      :base-graph ,graph
;;      :constraints ',(loop for (name . predicate-specifications) in type-specifications
;;                           for type-sub-constraint = (if (eq name '_) nil `(:type ,name))
;;                           if predicate-specifications
;;                             append (loop for (direction . predicates) in predicate-specifications
;;                                          for type-constraint
;;                                            = (when type-sub-constraint
;;                                                (case direction
;;                                                  (-> `(:subject ,type-sub-constraint))
;;                                                  (<- `(:object ,type-sub-constraint))
;;                                                  (otherwise (error "Direction must be <- or -> but got ~s" direction))))
;;                                          append (loop for predicate in predicates
;;                                                       for predicate-constraint
;;                                                         = (if (eq predicate '_)
;;                                                               `()
;;                                                               `(:predicate (:value ,predicate)))
;;                                                       collect `(,@type-constraint ,@predicate-constraint)))
;;                           else
;;                             ;; shorthand for all predicates
;;                             collect (if name `(:subject ,type-sub-constraint) `())))))

;; (define-graph user-specific ("http://mu.semte.ch/graphs/user/")
;;   ("foaf:Person"
;;    (-> "foaf:firstName" "foaf:lastName" "schema:email" "foaf:mbox")
;;    (<- "veeakker:hasBasket"))
;;   ("foaf:OnlineAccount"
;;    (-> "foaf:accountServiceHomepage" "foaf:accountName")
;;    (<- "foaf:holdsAccount")))

;; (define-graph something-specific ("http://mu.semte.ch/graphs/external-audit-trails/")
;;   (_ (-> "ext:auditTrail")))

;; (define-graph everything ("http://mu.semte.ch/application")
;;   (_ (-> _)))

(defmacro define-graph (name (graph) &body type-specifications)
  "Compact DSL for specifying common graph constraints."
  `(add-or-replace-graph
    (make-graph-specification
     :name ',name
     :base-graph ,graph
     :constraints ',(loop for (type-name . predicate-specifications) in type-specifications
                          for type-sub-constraint = (if (eq type-name '_) nil `(:type ,type-name))
                          if predicate-specifications
                            append (loop for (direction predicate) on predicate-specifications
                                           by #'cddr
                                         for type-constraint
                                           = (when type-sub-constraint
                                               (case direction
                                                 (-> `(:subject ,type-sub-constraint))
                                                 (<- `(:object ,type-sub-constraint))
                                                 (otherwise (error "Direction must be <- or -> but got ~s" direction))))
                                         for predicate-constraint
                                           = (if (eq predicate '_)
                                                 `()
                                                 `(:predicate (:value ,predicate)))
                                         collect `(,@type-constraint ,@predicate-constraint))
                          else
                            ;; shorthand for all predicatse
                            collect (if type-name `(:subject ,type-sub-constraint) `())))))

(define-graph user-specific ("http://mu.semte.ch/graphs/user/")
  ("foaf:Person"
   -> "foaf:firstName"
   -> "foaf:lastName"
   -> "schema:email"
   -> "foaf:mbox"
   <- "veeakker:hasBasket")
  ("foaf:OnlineAccount"
   -> "foaf:accountServiceHomepage"
   -> "foaf:accountName"
   <- "foaf:holdsAccount"))

(define-graph something-specific ("http://mu.semte.ch/graphs/external-audit-trails/")
  (_ -> "ext:auditTrail"))

(define-graph everything ("http://mu.semte.ch/application")
  (_ -> _))


(grant (read write)
       :to-graph user-specific
       :for-allowed-group "user")

(grant (read write)
       :to user-specific
       :for-token "user")

(grant read
       :to public
       :for-token "public")

(grant read
       :to delivered-orders
       :for-token "user")

(with-scope "service:image-service"
  (grant (read write)
         :to files
         :for-token "public"))

(supply-token "public")

(supply-token "user"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          PREFIX musession: <http://mu.semte.ch/vocabularies/session/>
          SELECT ?id WHERE {
           <SESSION_ID> ext:hasAccount/mu:uuid ?id.
          }"
  :parameters ("id"))

;; (setf *rights*
;;       (list (make-access-grant
;;              :usage '(:read :write)
;;              :graph-spec 'public-data
;;              :access "public")
;;             (make-access-grant
;;              :usage '(:read :write)
;;              :graph-spec 'user-specific
;;              :access "user")))

;; granting access to groups within scopes

;; ;; anyone can read public
;; (grant :read :to public :for application)
;; ;; image service can do more
;; (grant (:read :write)
;;        :to public :for application
;;        :scope "http://mu.semte.ch/services/image-service")
;; ;; users can read and write their own data
;; (grant (:read :write) :to user :for private) 

;; (supply-token "user"
;;               :when (query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
;;                             PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
;;                             PREFIX musession: <http://mu.semte.ch/vocabularies/session/>
;;                             SELECT ?id WHERE {
;;                              <SESSION_ID> ext:hasAccount/mu:uuid ?id.
;;                             }")
;;               :parameters ("id"))

;; (define-user-group account
;;   :access (by-query :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
;;                             PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
;;                             PREFIX musession: <http://mu.semte.ch/vocabularies/session/>
;;                             SELECT ?id WHERE {
;;                              <SESSION_ID> ext:hasAccount/mu:uuid ?id.
;;                             }"
;;                     :vars ("id")))

;; (defmacro define-graph (graph (&rest accounts)))

;; (define-graph "http://data.toevla.org/accounts/"
;;   :user-groups (list account)
;;   )

;; (defclass )

;; (defun user-groups-for-session (session-uri scope)
;;   "Calculates user groups for the current session uri."
;;   (remove-if-not (lambda (ug)
;;                    (group-applies group session-uri scope))
;;                  *user-groups*))

;; let's first cope with cached user groups
 

;;;; An example for veeakker
;; (setf *access-specifications*
;;       (list (make-instance 'always-accessible
;;                            :name "public")
;;             (make-instance 'access-by-query
;;                            :name "admin"
;;                            :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
;;                                    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
;;                                    PREFIX veeakker: <http://veeakker.be/vocabularies/shop/>

;;                                    SELECT ?id WHERE {
;;                                      <SESSION_ID> session:account/mu:hasRole veeakker:administrator.
;;                                    }")
;;             (make-instance 'access-by-query
;;                            :name "user"
;;                            :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
;;                                    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
;;                                    SELECT ?id WHERE {
;;                                      <SESSION_ID> session:account/mu:uuid ?id.
;;                                    }"
;;                            :vars (list "id"))))

;; (setf *graphs*
;;       (list (make-graph-specification
;;              :name 'public-data
;;              :base-graph "http://mu.semte.ch/graphs/public"
;;              :constraints '((:subject (:type "http://schema.org/Organization"))
;;                             (:subject (:type "http://veeakker.be/vocabularies/shop/DeliveryPlace"))
;;                             (:subject (:type "http://veeakker.be/vocabularies/shop/DeliveryKind"))
;;                             (:subject (:type "http://schema.org/GeoCoordinate"))
;;                             (:subject (:type "http://schema.org/PostalAddress"))
;;                             (:subject (:type "http://veeakker.be/vocabularies/shop/ProductGroup"))
;;                             (:subject (:type "http://schema.org/Product"))
;;                             (:subject (:type "http://purl.org/goodrelations/v1#Offering"))
;;                             (:subject (:type "http://purl.org/goodrelations/v1#UnitPriceSpecification"))
;;                             (:subject (:type "http://purl.org/goodrelations/v1#QuantitativeValue"))
;;                             (:subject (:type "http://purl.org/goodrelations/v1#TypeAndQuantityNode"))
;;                             (:subject (:type "http://veeakker.be/vocabularies/shop/SpotlightProduct"))
;;                             (:subject (:type "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject"))
;;                             (:subject (:type "http://mu.semte.ch/vocabularies/ext/Banner"))))
;;             (make-graph-specification
;;              :name 'user-specific
;;              :base-graph "http://mu.semte.ch/graphs/user/"
;;              ;; it seems we don't actually know the constraints
;;              :constraints '((:subject (:type "http://xmlns.com/foaf/0.1/Person")
;;                              :predicate (:value "http://xmlns.com/foaf/0.1/mbox"))))
;;             (make-graph-specification
;;              :name 'clean
;;              :base-graph "http://mu.semte.ch/graphs/nothing/"
;;              :constraints '())))

;; (setf *rights*
;;       (list (make-access-grant
;;              :usage '(:read)
;;              :graph-spec 'public-data
;;              :access "public")
;;             (make-access-grant
;;              :usage '(:read :write)
;;              :graph-spec 'public-data
;;              :access "admin")
;;             (make-access-grant
;;              :usage '(:read :write)
;;              :graph-spec 'user-specific
;;              :access "user")
;;             (make-access-grant
;;              :usage '(:read)
;;              :graph-spec 'clean
;;              :access "clean")))

;; An example for lokaalbeslist.vlaanderen.be
;; (setf *access-specifications*
;;       (list (make-instance 'always-accessible
;;                            :name "public")))

;; (setf *graphs*
;;       (list (make-graph-specification
;;              :name 'public-data
;;              :base-graph "http://mu.semte.ch/graphs/public"
;;              :constraints '())
;;             (make-graph-specification
;;              :name 'clean
;;              :base-graph "http://mu.semte.ch/graphs/nothing/"
;;              :constraints '())))

;; (setf *rights*
;;       (list (make-access-grant
;;              :usage '(:read)
;;              :graph-spec 'public-data
;;              :access "public")
;;             (make-access-grant
;;              :usage '(:read)
;;              :graph-spec 'clean
;;              :access "clean")))


