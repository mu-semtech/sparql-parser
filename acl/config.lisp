(in-package :acl)

(define-prefixes
  :foaf "http://xmlns.com/foaf/0.1/")

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
       :for-allowed-group "user")

(grant read
       :to public
       :for-allowed-group "public")

(grant read
       :to delivered-orders
       :for-allowed-group "user")

(with-scope "service:image-service"
  (grant (read write)
         :to files
         :for-allowed-group "public"))

(supply-allowed-group "public")

(supply-allowed-group "user"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          PREFIX musession: <http://mu.semte.ch/vocabularies/session/>
          SELECT ?id WHERE {
           <SESSION_ID> ext:hasAccount/mu:uuid ?id.
          }"
  :parameters ("id"))
