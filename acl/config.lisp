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

(setf *access-specifications*
      (list (make-instance 'always-accessible :name 'public)
            (make-instance 'access-by-query
                           :name 'user
                           :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
                                   PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                                   SELECT ?id WHERE {
                                     <SESSION_ID> session:account/mu:uuid ?id.
                                   }"
                           :vars (list "id"))))

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
(setf *graphs*
      (list (make-graph-specification
             :name 'public
             :base-graph "http://mu.semte.ch/graphs/public"
             :constraints nil)
            (make-graph-specification
             :name 'user
             :base-graph "http://mu.semte.ch/graphs/user/"
             :constraints nil)))
;; (define-graph application ("http://mu.semte.ch/application")
;;   ("nfo:FileDataObject"))
;;
;; (define-graph private ("http://mu.semte.ch/graphs/user/")
;;   ("foaf:Person" :predicates ("foaf:givenName" "foaf:familyName" "foaf:mbox"))
;;   ("veeakker:Basket"))

(setf *rights*
      (list (make-access-grant
             :usage '(:read)
             :graph-spec 'public
             :access 'public)))

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
 
