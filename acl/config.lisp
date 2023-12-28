(in-package :acl)

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
