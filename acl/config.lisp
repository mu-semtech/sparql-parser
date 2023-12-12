(in-package :acl)

(defparameter *prefixes* nil
  "plist of prefixes with their expansion.")

(defun define-prefix (prefix expansion)
  "Defines a new prefix"
  (alexandria:appendf *prefixes* (list prefix expansion)))

(defmacro define-prefixes (&body body)
  `(progn ,@(loop for (prefix expansion) on body
                  by #'cddr
                  collect `(define-prefix ,prefix ,expansion))))

(define-prefixes
  :skos "http://www.w3.org/2004/02/skos/core#"
  :schema "http://schema.org/")

;;;; User groups
;;;;
;;;; The user group itself is something defining a certain access right.
;;;; This access right itself can be translated to one or more graps to
;;;; which data must be written or from which data may be read.
;;;;
;;;; Access rights make the assumption that two access rights can't
;;;; overlap.  We prefer te err on the side of caution and not allow
;;;; developers to construct code which would allow them to execute such
;;;; things, even though this could solve some cases.  This limitation
;;;; might be lifted in the future but code may currently make some
;;;; assumptions.
;;;;
;;;; It is desired to generate affected groups from a sudo query.  Hence
;;;; the group definition must be able to determine the affected groups
;;;; when a graph url is given, as well as determine the graph (if any)
;;;; from the groups.
;;;;
;;;; User groups can ideally be scoped further based on scopes.  A scope
;;;; is triggered by the producing microservice.  Said microservice can
;;;; determine the scope and find out what is necessary.  

;; (setf *access-specifications*
;;       (list (make-instance 'always-accessible :name "public")
;;             (make-instance 'access-by-query
;;                            :name 'user ; I think this should be "user" instead to match the access below
;;                            :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
;;                                    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
;;                                    SELECT ?id WHERE {
;;                                      <SESSION_ID> session:account/mu:uuid ?id.
;;                                    }"
;;                            :vars (list "id"))))

;; access rights
;;
;; Which mu-auth-allow-groups do you have?
;; (define-access public 'always-accessible)
;;
;; (define-access user 'access-by-query
;;   :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
;;           PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
;;           SELECT ?id WHERE {
;;             <SESSION_ID> session:account/mu:uuid ?id.
;;           }"
;;   :vars ("id"))

;; graphs in the store
;;
;; What data resides where?
;; (setf *graphs*
;;       (list (make-graph-specification
;;              :name 'public-data
;;              :base-graph "http://mu.semte.ch/graphs/public"
;;              :constraints '((:predicate (:value "http://bar"))
;;                             (:subject (:type "http://subject-type"))
;;                             (:object (:type "http://object-type"))))
;;             (make-graph-specification
;;              :name 'user-specific
;;              :base-graph "http://mu.semte.ch/graphs/user/"
;;              :constraints '((:subject (:type "http://xmlns.com/foaf/0.1/Person")
;;                              :predicate (:value "http://xmlns.com/foaf/0.1/mbox"))
;;                             (:subject (:type "http://xmlns.com/foaf/0.1/Person")
;;                              :predicate (:value "http://xmlns.com/foaf/0.1/firstName"))
;;                             (:subject (:type "http://xmlns.com/foaf/0.1/Person")
;;                              :predicate (:value "http://xmlns.com/foaf/0.1/lastName"))))
;;             (make-graph-specification
;;              :name 'clean
;;              :base-graph "http://mu.semte.ch/ext/nothing"
;;              :constraints nil)))
;; (define-graph application ("http://mu.semte.ch/application")
;;   ("nfo:FileDataObject"))
;;
;; (define-graph private ("http://mu.semte.ch/graphs/user/")
;;   ("foaf:Person" :predicates ("foaf:givenName" "foaf:familyName" "foaf:mbox"))
;;   ("veeakker:Basket"))
;;
;; (define-graph user-specific ("http://mu.semte.ch/graphs/user")
;;   (foaf:Person
;;      (-> "foaf:firstName" "foaf:lastName" "schema:email" "foaf:mbox")
;;      (<- "veeakker:hasBAsket"))
;;   (foaf:OnlineAccount
;;      (-> "foaf:accountServiceHomepage" "foaf:accountName")
;;      (<- "foaf:holdsAccount")))

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
