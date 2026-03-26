# SPARQL Parser
The SPARQL endpoint authorization service (SEAS) is a layer that is placed in front of a SPARQL endpoint and that rewrites queries on this endpoint based on the session information of the user and the access rights on the data.

The idea is that data is organized into graphs and the access to these graphs is restricted to a certain group of users. When a query request is sent to the SPARQL endpoint it is intercepted by SEAS. The SEAS then calculates the appropriate access tokens based on the information, such as the session, in the intercepted request. It then iterates over the calculated tokens to determine the accessible graphs per token. Finally, the intercepted query is performed on the of accessible graphs.

This is a rewritten implementation of [`mu-authorization`](https://github.com/mu-semtech/mu-authorization) in Common Lisp.

## Tutorials
### How to add the sparql-parser service to your application
Start by adding the service to your application's `docker-compose.yml`:

```yaml
services:
  database:
    image: semtech/sparql-parser:0.0.15
    volumes:
      - ./config/authorization:/config
      - ./data/authorization:/data
```

**NOTE:** If necessary, change the name of your triplestore service to something other than `database`.

Next, create a configuration file `./config/authorization/config.lisp`. In this file you can configure the sparql-parser service by setting variables to appropriate values and defining the access control policy for your application. For example, the following snippet first configures a SPARQL endpoint service as the `*backend*` to which sparql-parser will talk. Note that `triplestore` here is the name of the endpoint service as set in your application's `docker-compose.yml`. Next it enables logging some extra information to the standard output by setting two variables to `t`. Finally, it enables the generation of delta messages for data changes caused by insert or delete queries.

```lisp
;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *backend* "http://triplestore:8890/sparql")

(setf *log-sparql-query-roundtrip* t)
(in-package :server)
(setf *log-incoming-requests-p* t)

;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

(add-delta-logger)
(add-delta-messenger "http://delta-notifier/")
```

Now you can start your stack using `docker compose up -d`. At this point you will likely not see any data in your application as you have not yet configured any access rights. Adding the following snippet to the `config.lisp` configures read access for everyone for all data in the `http://mu.semte.ch/graphs/public` graph.

```lisp
;;;;;;;;;;;;;;;;;
;;; access rights
(in-package :acl)

(define-graph public ("http://mu.semte.ch/graphs/public")
  (_ -> _))

(supply-allowed-group "public")

(grant (read)
  :to-graph public
  :for-allowed-group "public")
```

To load the added the access policy, restart your service using `docker compose restart database`. After restarting, data should show up when in your application. Consult the how-to guides in the following section for more information in defining more meaningful access control policies.

## How-to guides
### Define a group for users with a certain role
An access control policy typically grants different rights to users based on some criteria. For example, an authenticated user may read and edit certain data, whereas other users are only allowed to read data. This requires that we can determine to which group(s) the user performing a request belongs to. In a sparql-parser configuration is done using the `supply-allowed-group` macro. This macro supports defining SPARQL queries to determine whether a user belongs to a group. More specifically, the provided query should return a match when a user belongs to the defined group.

Say you want to define a group that contains all authenticated users. In a semantic.works application this usually means that there exists a session associated with an account, indicating that the user previously logged in. The following snippet defines a group named `authenticated` where membership is determined by the existence of a session associated with an account:

```lisp
(in-package :acl)

(supply-allowed-group "authenticated"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>

          SELECT DISTINCT ?account WHERE {
            <SESSION_ID> session:account ?account.
          }")
```

Note that the constant `SESSION_ID` is a placeholder and will be automatically replaced by the actual session identifier found in the request when the query is executed.

For users with a certain role, say `SuperMegaAdmin`, a similar query can be used which filters based on the role associated with a session:

```lisp
(supply-allowed-group "super-mega-admins"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

          SELECT DISTINCT ?session_role WHERE {
            <SESSION_ID> ext:sessionRole ?session_role .
            FILTER( ?session_role = \"SuperMegaAdmin\" )
          }")
```

While it is rather common to define group membership based on roles, sparql-parser is not limited to this and allows arbitrary queries to be specified. It depends on your application's data model which queries make sense. Note, in the above examples the actual matches returned by the queries are not used, [another guide](#define-access-rights-for-a-set-of-similar-graphs) shows how you can use these matches to simplify access control policies in some situations.

### Use compact URIs by defining prefixes
You will often need to write URIs for resources, predicates, etc. while specifying access control policies for sparql-parser. As it is cumbersome to always write full URIs, and this also negatively impacts the readability of a configuration, sparql-parser supports using Compact URIs or [CURIEs](https://www.w3.org/TR/curie/). For this you need to define the prefixes you want to use along with their corresponding expansions.

Prefixes are defined using the `define-prefixes` macro whose body is a sequence of keyword/value pairs of the form `:PREFIX "EXPANSION"`. For example, to be able to write `foaf:name` instead of `http://xmlns.com/foaf/0.1/name` you can define the `foaf:` prefix as follows:

```lisp
(in-package :acl)
(define-prefixes
  :foaf "http://xmlns.com/foaf/0.1/")
```

Note that the keyword `:foaf` does **not** contain a trailing colon ':' as would be the case in other languages such as [SPARQL](https://www.w3.org/TR/sparql11-query/#prefNames) or [TTL](https://www.w3.org/TR/turtle/#sec-iri). The colon preceding a keyword is required for it to be considered a keyword in the underlying data structure in which the pair is inserted.

Be sure to define prefixes **before** their first use in some other part of your configuration. Otherwise, you will encounter errors when starting sparql-parser.

You can define multiple prefixes in one go by simply putting multiple keyword/value pairs in the body of `define-prefixes`:

```lisp
(in-package :acl)
(define-prefixes
  :foaf "http://xmlns.com/foaf/0.1/"
  :adms "http://www.w3.org/ns/adms#"
  :cal "http://www.w3.org/2002/12/cal/ical#"
  :cogs "http://vocab.deri.ie/cogs#"
  :dcat "http://www.w3.org/ns/dcat#"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :eli "http://data.europa.eu/eli/ontology#")
```

**NOTE**: Be aware that the defined prefixes do **not** affect prefixes that can be used in SPARQL query strings such as those defined in the [previous section](#define-a-group-for-users-with-a-certain-role). In such queries prefixes still need to be specified using the `PREFIX` keyword.

### Define which triples are accessible for a graph
Typically you want to explicitly specify which (kind of) triples within a graph an access control rule can be applied to. In sparql-parser such information is captured by *graph-specifications* which you create using the `define-graph` macro. For readability the code snippets use CURIEs as explained in the guide on [defining prefixes](#use-compact-uris-by-defining-prefixes).

For instance, say you have a graph `http://mu.semte.ch/graphs/people` containing triples for resources of types `foaf:Person` and `foaf:OnlineAccount`. The following snippet creates a graph-specification for that graph which covers all triples for resources of these two types. Here `people` is a unique identifier by which this graph-specification can be referred to later on. The URI of the target graph, `http://mu.semte.ch/graphs/people`, is specified as a string between brackets and double quotes.

```lisp
(in-package :acl)
(define-graph people ("http://mu.semte.ch/graphs/people")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _))
```

The remaining elements, `("foaf:Person" -> _)` and `("foaf:OnlineAccount" -> _)`, are so-called *type-specifications*. A type-specification specifies which triples are considered relevant for a certain resource type. More specifically, `("foaf:Person" -> _)` means that triples with a subject of type `foaf:Person` and any predicate are relevant for this graph-specification. The `_` character is thus a wildcard that matches everything.

If you are interested in a more limited set of triples, you can explicitly specify one or more predicates instead of the the wildcard. For example, if you only want triples for `foaf:Person` that have as predicate `foaf:firstName` or `foaf:familyName` that can be written as shown below. Note, that the operator `->` is repeated for each individual predicate.

```lisp
(in-package :acl)
(define-graph people ("http://mu.semte.ch/graphs/people")
  ("foaf:Person" -> "foaf:firstName"
                 -> "foaf:familyName")
  ("foaf:OnlineAccount" -> _))
```

Alternatively, you may be interested in most triples for a resource type except those with a few specific predicates. While you can list all relevant predicates as above, sparql-parser provides another operator `x>` to describe such situations his more concisely. For example, if you are interested in all triples with a `foaf:OnlineAccount` resource as subject except those that have as predicate `ext:password` or `account:accountName`. This can be written as follows:

```lisp
(in-package :acl)
(define-graph people ("http://mu.semte.ch/graphs/people")
  ("foaf:Person" -> "foaf:firstName"
                 -> "foaf:familyName")
  ("foaf:OnlineAccount" x> "ext:password"
                        x> "account:accountName"))
```

So far the type-specifications only concerned triples with a *subject* of a specific resource type. To specify triples where the *object* is of a given resource type you can use the inverse operators `<-` and `<x`. For example, `("foaf:Person" <- "schema:employee")` means all triples that link an object of resource type `foaf:Person` to some subject resource via the predicate `schema:employee`. This can be added to the `define-graph` snippet as follows:

```lisp
(in-package :acl)
(define-graph people ("http://mu.semte.ch/graphs/people")
  ("foaf:Person" -> "foaf:firstName"
                 -> "foaf:familyName"
                 <- "schema:employee")
  ("foaf:OnlineAccount" x> "ext:password"
                        x> "account:accountName"))
```

In summary this graph-specification contains all triples in `http://mu.semte.ch/graphs/people` that have

- as *subject* a resource of type `foaf:Person` AND as *predicate* `foaf:firstName` or `foaf:familyName`; OR
- has as *object* a resource of type `foaf:Person` AND as *predicate* `schema:employee`; OR
- as *subject* a resource of type `foaf:OnlineAccount` AND **not** as *predicate* `ext:password` or `account:accountName`.

### Granting a group rights to a graph
Once you have defined the necessary [access-groups](#define-a-group-for-users-with-a-certain-role) and [graph-specifications](#define-which-triples-are-accessible-for-a-graph) you can grant rights using the `grant` macro. This macro expects as input a list of granted rights, the target graph-specification(s), and access-group(s). For example, the following snippets grants users that are members of the `authenticated` group read rights to the triples in the `people` graph-specification.

```lisp
(in-package :acl)
(grant (read)
  :to-graph people
  :for-allowed-group "authenticated")
```

To grant multiple rights you can simply list them in the first argument. For instance, the following snippet grants users in the `super-mega-admin` group read and write rights to triples in the `people` graph-specification. Note, that `read` and `write` are currently the only supported rights.

```lisp
(in-package :acl)
(grant (read write)
  :to-graph people
  :for-allowed-group "super-mega-admins")
```

It is supported to provide multiple target graph-specifications and/or access-groups by surrounding the corresponding argument with brackets and listing multiple values. For example, users in the `super-mega-admins` access-group can be granted rights to the graph-specifications `people` and `organization` as follows:

```lisp
(in-package :acl)
(grant (read write)
  :to-graph (people organization)
  :for-allowed-group "super-mega-admins")
```

### Define access rights for a set of similar graphs
Your application may have multiple graphs whose contents are structurally similar in that they overlap in terms of resource types and predicates. For example, your application might have a single graph per organization where each graph contains similar triples such as the organization's name, address and employees.

For such situations sparql-parser supports defining the access rights only once for all graphs together, instead of having to define them individually for each graph and group separately. First, this requires specifying a `:parameters` argument for the relevant access-groups as shown in the snippet below. This argument expects list of strings that is a subset of the variables specified in the `SELECT` clause of the corresponding query.

```lisp
(in-package :acl)
(supply-allowed-group "organization-member"
  :parameters ("session_group")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
          }")
```

Let's assume that the graphs for the different organizations have URIs for the form `http://mu.semte.ch/graphs/organizations/UUID`, where UUID identifies the specific organization this graph pertains to. The following graph-specification would cover all such graphs. Note that the provided graph URI argument does **not** contain the UUID part.

```lisp
(in-package :acl)
(define-graph organization ("http://mu.semte.ch/graphs/organizations/")
  ("org:Organization" -> _)
  ("foaf:Person" -> "foaf:firstName"
                 -> "foaf:familyName"))
```

To grant read access to members of the `organization-member` group to the `organization` graph-specification the following `grant` can be defined.

```lisp
(in-package :acl)
(grant (read)
       :to-graph organization
       :for-allowed-group "organization-member")
```

The "magic" here happens when sparql-parser processes an appropriate request, i.e. a request from a member of `organization-member` group for triples in the `organization` graph-specification. In such cases sparql-parser determines the target graphs by appending the match(es) for the `session_group` parameter to the graph URI in the `organization` graph-specification. For example, say the query in `organization-member` returns two matches for `session_group`: `someOrganization` and `aCompletelyDifferentOrganization`.  The incoming request will then be forward to two graphs with as URIs:

- `http://mu.semte.ch/graphs/organizations/someOrganization`
- `http://mu.semte.ch/graphs/organizations/aCompletelyDifferentOrganization`

If you specify multiple values in a group's `:parameters` argument, their matches will be appended to graph URIs in the given order. For example, you can add `session_role` as a second `:parameters` argument to the above `organization-member` access-group as follows:

```lisp
(in-package :acl)
(supply-allowed-group "organization-member"
  :parameters ("session_group" "session_role")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
          }")
```

Let's say that the group's query returns the following matches for your application:

| session_group                    | session_role              |
|----------------------------------|---------------------------|
| someOrganization                 | someRole                  |
| aCompletelyDifferentOrganization | aCompletetlyDifferentRole |

In this case sparql-sparser will use the following graph URIs to forward requests for the `organization` graph-specification:

- `http://mu.semte.ch/graphs/organizations/someOrganization/someRole`
- `http://mu.semte.ch/graphs/organizations/aCompletelyDifferentOrganization/aCompletetlyDifferentRole`

### Generating delta messages for data changes
Sparql-parser can be configured to generate delta messages when quads are inserted or deleted. Such delta messages can be further distributed to interested parties using a [delta-notifier](https://github.com/mu-semtech/delta-notifier) service. To enable generating delta messages add the following to your configuration for sparql-parser.

```lisp
(in-package :delta-messenger)
(add-delta-messenger "http://delta-notifier/")
```

Here `delta-notifier` is the name of the delta-notifier service in your application as defined in `docker-compose.yml`. If this service is called differently in your case, modify the string accordingly.

If you also want to log delta messages to the standard output add the following to your configuration as well well.

```lisp
(in-package :delta-messenger)
(add-delta-logger)
```

The `define-graph` macro supports more fine-grained control for which graph-specifications delta messages should be generated. The `:delta` keyword parameter allows to disable delta messages for a specific graph-specification:

```lisp
(define-graph organization ("http://mu.semte.ch/graphs/organizations/" :delta nil)
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" x> "ext:password"))
```

Any other value than `nil` will be interpreted as `t`, which is the default value, and will enable delta messages for the graph specification.

### Enable additional logging
By default sparql-parser only logs to its standard output when requests fail to execute. To log more information different variables can be set to non-nil values.

The following snippet enables logging the queries sent to the SPARQL endpoint as well as the responses received from it.

```lisp
(in-package :client)
(setf *log-sparql-query-roundtrip* t)
```

If you want to log the requests and associated access rights that arrive at sparql-sparser add the following snippet to your configurations:

```lisp
(in-package :server)
(setf *log-incoming-requests-p* t)
```

### Handle resources without an explicit type
Possibly your application's data contains resources that do not have an explicit resource type. Consequently, such resources cannot be directly used in type-specifications in the body of graph-specifications. To work around this sparql-parser supports assuming certain types for resources based on their URI.

For example, say you have a graph `http://mu.semte.ch/graphs/sessions` which contains session resources whose URIs start with `http://mu.semte.ch/sessions/`. The following snippet essentially tells sparql-parser to assume such resources have as type `http://mu.semte.ch/vocabularies/session/Session`.

```lisp
(in-package :type-cache)
(add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session")
```

This allows using that type in graph-specifications the same as other resources:

```lisp
(in-package :acl)
(define-graph sessions ("http://mu.semte.ch/graphs/sessions")
  ("http://mu.semte.ch/vocabularies/session/Session" -> _))
```

**NOTE**: this is not strictly limited to resources without a type. But can also be used to assume additional types for resources next to those types  explicitly specified in the data.

### Define access rights for specific services
It is likely that in your semantic.works application not all requests sent to the SPARQL endpoint are (indirectly) triggered by users with a session. For example, a service may periodically and autonomously retrieve triples from the endpoint. In such cases, requests are not associated with a session from which the appropriate access-groups can be determined. Sparql-parser supports *scopes** which facilitate defining access control rules for such scenarios.

**NOTE**: This requires the service to which rights are granted is created with [mu-javascript-template](https://github.com/mu-semtech/mu-javascript-template) v1.9.0 or newer. Services based on older templates should first be upgraded or can use [mu-auth-sudo](https://github.com/lblod/mu-auth-sudo) as alternative solution.

For instance, let's assume your application has the following access control policy:

```lisp
(in-package :acl)

(supply-allowed-group "authenticated"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>

          SELECT DISTINCT ?account WHERE {
            <SESSION_ID> session:account ?account.
          }")

(define-graph people ("http://mu.semte.ch/graphs/people")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _))

(grant (read write)
       :to people
       :for "authenticated")
```

Now say you have a service `peopleservice` in your application which requires periodically retrieve the names of the `foaf:Person`s in the `people` graph. In your `docker-compose.yml` entry for this service, specify a value for the `DEFAULT_MU_AUTH_SCOPE` environment variable. The `peopleservice` will supply this value in the header of each outgoing request.

```yaml
services:
  peopleservice:
    image: example/peopleservice:0.0.1
    environment:
      DEFAULT_MU_AUTH_SCOPE: "http://services.semantic.works/people-service"
```

In your sparql-parser configuration you can use the `with-scope` macro to grant rights within a scope. For instance, the following snippet essentially states that the grant is also applicable for requests with the scope `"http://services.semantic.works/people-service"`.

```lisp
(with-scope "http://services.semantic.works/people-service"
  (grant (read write)
         :to people
         :for "authenticated"))
```

As an alternative notation you can use the `:scopes` keyword parameter for the `grant` macro as shown below. Note, that the argument value is surrounded by brackets and preceded by a quote `'`.

```lisp
(grant (read write)
       :to people
       :for "authenticated"
       :scopes '("http://services.semantic.works/example-service"))
```

Using the `:scopes` parameter notation it is possible to provide multiple scope URIs:

```lisp
(grant (read write)
       :to people
       :for "authenticated"
       :scopes '("http://services.semantic.works/people-service" "http://services.semantic.works/another-service"))
```


### Defining an authorization policy in ODRL

> [!WARNING]
> Support for ODRL policies is under development and some functionality, such as using scopes, is not yet (fully) supported.

This service also supports defining policies using [ODRL](https://www.w3.org/TR/odrl-model/), as an alternative to the lisp-style configuration illustrated above. To enable ODRL policies, set `*use-odrl-config-p*` to non-nil in the config file mounted in `./config/authorization/config.lisp` as shown below. Note, other service configuration settings, such as `*backend*`, should still be set in the same file.

```lisp
;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

(add-delta-logger)
(add-delta-messenger "http://delta-notifier/")

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil)

(in-package :odrl-config)
(setf *use-odrl-config-p* t)
```

The actual policy should be defined in a [Turtle](https://www.w3.org/TR/turtle) file mounted in `./config/authorization/config.ttl`. The following snippet contains the ODRL equivalent, encoded in Turtle format, for the lisp access rights as shown in the [first](#how-to-add-the-sparql-parser-service-to-your-application) in this README. The following subsections describe each part in more detail. Furthermore, a more comprehensive policy example can be found in the [test configuration]('./test/example-config.ttl').

```ttl
@prefix example: <http://www.example.org/> .
@prefix ext: <http://mu.semte.ch/vocabularies/ext/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .

example:examplePolicy a odrl:Set ;
  odrl:permission ext:publicRead .

example:publicGraph a odrl:AssetCollection ;
  vcard:fn "public" ;
  ext:graphPrefix <http://mu.semte.ch/graphs/public> .

example:genericAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:publicGraph ;
  sh:targetClass ext:all .

example:publicParty a odrl:PartyCollection ;
  vcard:fn "public" .

example:publicRead a odrl:Permission ;
  odrl:action odrl:read ;
  odrl:target ext:publicGraph ;
  odrl:assignee ext:publicParty .
```


The following functionality is *not* yet supported when using an ODRL policy:
- Specifying `scopes` for a permission.
- Specifying an explicit `constraint` for an `allowed-group`, currently this is implicitly set based on whether a query is provided or not.
- Specifying options, such as whether to generate deltas, per graph definition.

Furthermore, [policy rule composition](https://www.w3.org/TR/odrl-model/#composition) is *not* yet supported. So each rule should be specified using its normative cardinalities for property relationships.


#### Define a group for users with a certain role in ODRL
An access control policy typically grants different rights to users based on some criteria. For example, an authenticated user may read and edit certain data, whereas other users are only allowed to read data. This requires that we can determine to which group(s) the user performing a request belongs to. In an ODRL configuration this captured by defining a party collection resource. Such a resource should at least have a `vcard:fn` property that specifies the name of the group. The `ext:definedBy` property allows to specify a SPARQL query with which to determine whether a user belongs to a group. More specifically, the provided query should return a match when a user belongs to the defined group.

Say you want to define a group that contains all authenticated users. In a semantic.works application this usually means that there exists a session associated with an account, indicating that the user previously logged in. The following snippet defines a party collection for group named `authenticated` where membership is determined by the existence of a session associated with an account:

```ttl
@prefix example: <http://www.example.org/> .
@prefix ext: <http://mu.semte.ch/vocabularies/ext/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .

example:authenticatedUserParty a odrl:PartyCollection ;
  vcard:fn "authenticated" ;
  ext:definedBy """PREFIX session: <http://mu.semte.ch/vocabularies/session/>

          SELECT DISTINCT ?account WHERE {
            <SESSION_ID> session:account ?account.
          }""" .
```

Note that the constant `SESSION_ID` is a placeholder and will be automatically replaced by the actual session identifier found in the request when the query is executed.

#### Define which triples are accessible for a graph in ODRL
Typically you want to explicitly specify which (kind of) triples within a graph an access control rule can be applied to. In an ODRL configuration such information is captured by an Asset collection along with its contained assets.

For instance, say you have a graph `http://mu.semte.ch/graphs/people` containing triples for resources of types `foaf:Person` and `foaf:OnlineAccount`. The following snippet defines an asset collection `example:peopleGraph`. The `vcard:fn` property specifies the name for this asset collection. This name should be unique as it will be used internally to identify this asset collection. The `ext:graphPrefix` property has as value the URI of the graph the asset collection refers to.

```ttl
@prefix example: <http://www.example.org/> .
@prefix ext: <http://mu.semte.ch/vocabularies/ext/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .


example:peopleGraph a odrl:AssetCollection ;
  vcard:fn "people" ;
  ext:graphPrefix <http://mu.semte.ch/graphs/people> .

example:foafPersonAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:peopleGraph ;
  sh:targetClass foaf:Person .

example:foafOnlineAccountAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:peopleGraph ;
  sh:targetClass foaf:OnlineAccount .
```

The two assets `example:foafPersonAsset` and `example:foafOnlineAccountAsset` specify the relevant triples in a graph. The `odrl:PartOf` property specifies which asset collection(s) the asset belongs to. Note, that these assets are also assigned the type `sh:NodeShape`. This is because we use [SHACL](https://www.w3.org/TR/shacl/) shapes the define exact triples covered by an asset. The simplest case is to specify a resource type as value of the `sh:targetClass` property. This means that the asset covers all triples with a subject resource of the specified type. More concretely, for the `example:foafPersonAsset` this means that asset covers all triples whose subject is a resource of type `foaf:Person`. Keep in mind that the triples for an asset should always be considered with respect to the asset collection(s) it is part of. More concretely, the above `example:foafOnlineAccountAsset` only covers triples in the graph the corresponds to the `example:peopleGraph` it is part of.

If you are interested in a more limited set of triples, you can explicitly specify one or more predicates using SHACL property shapes. This can be achieved by defining the appropriate values for `sh:property` properties. For example, say you only want to cover triples for `foaf:Person` resources that have as predicate `foaf:firstName` or `foaf:familyName`. In that case you can specify two property nodes, one for each predicate, as shown for `example:foafPersonAssetOnlyName` below.

```ttl
@prefix example: <http://www.example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

example:foafPersonNamesOnlyAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:peopleGraph ;
  sh:targetClass foaf:Person ;
  sh:property [ sh:path foaf:firstName ] ,
      [ sh:path foaf:familyName ] .
```

Alternatively, you may be interested in most triples for a resource type except those with a few specific predicates. While you can list all relevant predicates as above, sparql-parser supports a shorter notation to describe such situations more concisely. Similar to above this uses SHACL property shapes to specify the desired predicates, but surrounding them with a `sh:not` logical constraint component. For example, say you are interested in all triples with a `foaf:OnlineAccount` resource as subject, except those triples that have as predicate `ext:password` or `account:accountName`. This can be specified as shown in the `example:foafOnlineAccountAsset` shown below.

```lisp
@prefix example: <http://www.example.org/>
@prefix ext: <http://mu.semte.ch/vocabularies/ext/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

example:foafOnlineAccountAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:peopleGraph ;
  sh:targetClass foaf:OnlineAccount ;
  sh:not [
    sh:property [ sh:path ext:password ],
        [ sh:path foaf:accountName ]
  ] .
```

So far the assets only concerned triples with a *subject* of a specific resource type. To specify triples where the *object* is of a given resource type you can use property shapes with an `sh:inversePath` as property path. For example, the `example:foafPersonObjectAsset` below covers all triples which have an object of type `foaf:Person`. Here the `ext:all` object acts as a wildcard value meaning all predicates.

```lisp
@prefix example: <http://www.example.org/> .
@prefix ext: <http://mu.semte.ch/vocabularies/ext/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

example:foafPersonObjectAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:peopleGraph ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path [ sh:inversePath ext:all ]
  ] .
```

Similarly as before, you can also specify a concrete predicate for an inverse path to limit an asset to triples with an object of a certain *and* specific predicates. For example, the `example:foafPersonObjectEmployeeAsset` below covers triples that have a `foaf:Person` as object and have `schema:employee` as predicate.

```lisp
@prefix example: <http://www.example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix schema: <http://schema.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

example:foafPersonObjectEmployeeAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:peopleGraph ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path [ sh:inversePath schema:employee ]
  ] .
```

Note that you can combine regular and inverted paths in a single asset. For example, the `example:foafPersonComplexAsset` below covers all triples that have

- as *subject* a resource of type `foaf:Person` AND as *predicate* `foaf:firstName` or `foaf:familyName`; OR
- as *object* a resource of type `foaf:Person` AND as *predicate* `schema:employee`

```ttl
@prefix example: <http://www.example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix schema: <http://schema.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

example:foafPersonComplexAsset a odrl:Asset, sh:NodeShape ;
  odrl:partOf example:peopleGraph ;
  sh:targetClass foaf:Person ;
  sh:property [ sh:path foaf:firstName ] ,
      [ sh:path foaf:familyName ] ,
      [ sh:path [ sh:inversePath schema:employee ] ] .
```


#### Granting a group rights to a graph in ODRL
Once you have defined the necessary [party collections](#define-a-group-for-users-with-a-certain-role-in-odrl) and [asset collections](#define-which-triples-are-accessible-for-a-graph-in-odrl) you can grant rights by defining ODRL permissions. Each permission requires you define exactly one action, target asset collection, and assignee party collection. For example, the `example:peopleReadPermission` below grants users that are members of the `example:authenticatedUserParty` party collection read rights to the triples in the `example:peopleGraph` asset collection.

```ttl
@prefix example: <http://www.example.org/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .

example:peopleReadPermission a odrl:Permission ;
  odrl:action odrl:read ;
  odrl:target example:peopleGraph ;
  odrl:assignee example:authenticatedUserParty .
```

> [!IMPORTANT]
> Sparql-parser only supports the `odrl:read` and `odrl:modify` actions, specifying any other action will result in an error on loading the defined policy.

To grant multiple rights you have to specify multiple permissions, one for each allowed action. For instance, to grant members of the `example:authenticatedUserParty` party collection also write rights to the triples in the `example:peopleGraph` asset collection you have add a second permission as shown below.

```ttl
@prefix example: <http://www.example.org/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .

example:peopleReadPermission a odrl:Permission ;
  odrl:action odrl:read ;
  odrl:target example:peopleGraph ;
  odrl:assignee example:authenticatedUserParty .

example:peopleWritePermission a odrl:Permission ;
  odrl:action odrl:modify ;
  odrl:target example:peopleGraph ;
  odrl:assignee example:authenticatedUserParty .
```

Similarly, grant members of a party collection rights to multiple asset collections requires you specify one permission per target asset collection. For example, the following snippet grants members of the `example:authenticatedUserParty` party collection read rights to the `example:peopleGraph` asset collection as well as the `example:organizationGraph` asset collection.

```ttl
@prefix example: <http://www.example.org/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .

example:peopleReadPermission a odrl:Permission ;
  odrl:action odrl:read ;
  odrl:target example:peopleGraph ;
  odrl:assignee example:authenticatedUserParty .

example:organizationReadPermission a odrl:Permission ;
  odrl:action odrl:read ;
  odrl:target example:organizationGraph ;
  odrl:assignee example:authenticatedUserParty .
```

#### Define access rights for a set of similar graphs in ODRL
Your application may have multiple graphs whose contents are structurally similar in that they overlap in terms of resource types and predicates. For example, your application might have a single graph per organization where each graph contains similar triples such as the organization's name, address and employees.

For such situations sparql-parser supports defining the access rights only once for all graphs together, instead of having to define them individually for each graph and group separately. First this requires specifying one or more values for an `ext:queryParameters` property for a party collection. Note, that the literal(s) assigned as object value(s) must be a subset of the variables specified in the `SELECT` clause of the query specified in the `ext:definedBy` property.

```ttl
@prefix example: <http://www.example.org/> .
@prefix ext: <http://mu.semte.ch/vocabularies/ext/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .

example:organizationMemberParty a odrl:PartyCollection ;
  vcard:fn "organization-member" ;
  ext:queryParameters "session_group" ;
  ext:definedBy """PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
          }""" .
```

Let's assume that the graphs for the different organizations have URIs for the form `http://mu.semte.ch/graphs/organizations/UUID`, where UUID identifies the specific organization this graph pertains to. The following asset collection would cover all such graphs. Note that the provided graph URI specified as value for `ext:graphPrefix` ends with a "/" and does **not** contain the UUID part. (For brevity we do not specify any assets in this example.)

```ttl
example:organizationGraphs a odrl:AssetCollection ;
  vcard:fn "organization" ;
  ext:graphPrefix <http://mu.semte.ch/graphs/organizations/> .
```

To grant read access to members of the `example:organizationMemberParty` party collection to the `example:organizationGraphs` asset collection the following permission can be defined.

```ttl
@prefix example: <http://www.example.org/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .

example:organizationReadPermission a odrl:Permission ;
  odrl:action odrl:read ;
  odrl:target example:organizationGraphs ;
  odrl:assignee example:organizationMemberParty .
```

The "magic" here happens when sparql-parser processes an appropriate request, i.e. a request from a member of `example:organizationMemberParty` party collection for triples in the `example:organizationGraphs` asset collection. In such cases sparql-parser determines the target graphs by appending the match(es) for the `session_group` parameter to the graph URI in the `example:organizationGraphs` asset collection. For example, say the query in `example:organizationMemberParty` returns two matches for `session_group`: `someOrganization` and `aCompletelyDifferentOrganization`.  The incoming request will then be forward to two graphs with as URIs:

- `http://mu.semte.ch/graphs/organizations/someOrganization`
- `http://mu.semte.ch/graphs/organizations/aCompletelyDifferentOrganization`

If you want to specify multiple values for the `ext:queryParamters` you have to specify them as elements in a Turtle collection. The matches will be appended to graph URIs in the same order as the elements in the collection. For example, you can add `session_role` as a second `ext:queryParameters` argument to the above `example:organizationMemberParty` party collection as follows:

```ttl
@prefix example: <http://www.example.org/> .
@prefix ext: <http://mu.semte.ch/vocabularies/ext/> .
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .

example:organizationMemberParty a odrl:PartyCollection ;
  vcard:fn "organization-member" ;
  ext:queryParameters ( "session_group" "session_role" );
  ext:definedBy """PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
          }""" .
```

Let's say that the party collection's query returns the following matches for your application:

| session_group                    | session_role              |
|----------------------------------|---------------------------|
| someOrganization                 | someRole                  |
| aCompletelyDifferentOrganization | aCompletetlyDifferentRole |

In this case sparql-sparser will use the following graph URIs to forward requests for the `example:organizationGraphs` asset collection:

- `http://mu.semte.ch/graphs/organizations/someOrganization/someRole`
- `http://mu.semte.ch/graphs/organizations/aCompletelyDifferentOrganization/aCompletetlyDifferentRole`


#### Generating delta messages for data changes in ODRL
This functionality is not part of the ODRL policy itself. This should be configured in the `config.lisp` file as explained in [this guide](#generating-delta-messages-for-data-changes).

> [!WARNING]
> Policies in ODRL do not support enabling delta messages only for specific asset collections.

#### Enable additional logging in ODRL
This functionality is not part of the ODRL policy itself. This should be configured in the `config.lisp` file as explained in [this guide](#enable-additional-logging).

#### Define access rights for specific services in ODRL
Specifying scopes is **not** yet supported in ODRL policies. If you require this functionality you have to define you configuration in the lisp-style syntax.

## Reference
### ACL configuration interface
#### `define-graph`
A graph-specification essentially describes the set of triples in a graph to which rights can be assigned. It does this using one or more type-specifications. A type-specification in turn specifies a resource type and predicates that capture the relevant triples. A graph-specification is created using the `define-graph` macro:

```lisp
define-graph (name (graph &rest args &key (sparql t sparql-p) (delta t delta-p) (file nil file-p) (operations #'identity operations-p))
  &body type-specifications)
```

Parameters:
- *`name`* A symbol with which the created `graph-specification` can be referenced in the remainder of the configuration. The name should **not** blank spaces and should **not** be surrounded with (double) quotes.
- *`graph`* A string that is (the prefix for) a URI of a graph in which the triples are stored.

Keyword parameters:
- *`sparql`* If set to `nil` do **not** send SPARQL queries for this `graph-specification` to the backend. (default: `t`)
- *`delta`* If set to `nil` do **not** generate delta messages for changes for this `graph-specification`. (default: `t`)
- *`file`* Currently not used, intended for future functionality.
- *`operations`* Currently not used, intended for future functionality.

The *`body`* of a `define-graph` call contains one or more type-specifications. A `type-specification` has the form: `(<someResourceType> [<operator> <somePredicate>]+)`. `<someResourceType>` and `<somePredicate>` must be a URI string (e.g. `"http://xmlns.com/foaf/0.1/Person"` or `"foaf:Person"` if you have defined `foaf:` as a [prefix](#defining-prefixes)) or a `_` to indicate a wildcard.

There are four supported operations that can be used in a type-specification
- `T -> p`: Triples where the **subject** is of type `T` and the predicate is `p`.
- `T <- p`: Triples where the **object** is of type `T` and the predicate is `p`.
- `T x> p`: For triples where the **subject** is of type `T`, allow every predicate **except** for `p`.
- `T <x p`: For triples where the **object** is of type `T`, allow every predicate **except** for `p`.

#### `supply-allowed-group`
Groups to which access rights can be granted are defined using the `supply-allowed-group` macro.

```lisp
supply-allowed-group (group &body args &key constraint parameters query &allow-other-keys)
```

Parameters:
- *`group`* the name of the group as a string enclosed in double quotes, e.g. `"group-name"`

Keyword parameters:
- *`:query`* A string containing a SPARQL query that returns a result if a user belongs to the group at hand. Typically, a query will determine a user's group membership starting from their `<SESSION_ID>`. At runtime, `<SESSION_ID>` will be replaced by the value for `mu-session-id` in incoming requests.
- *`:parameters`* a list of strings that is a subset of the variable names used for matches returned by the query. Has no effect if no value is provided for `:query`.
- *`:constraint`* A symbol to specify how group membership should be determined. The supported values are:
  + `always`: All users belong to this group. Any specified `:query` (and `:parameters`) will be ignored.
  + `never`: No user can belong to this group, and it cannot be used in any access-grants. Any valued for `:query` and/or `:parameters` will be ignored.
  + `query`: Whether a user belongs to this group is determined by the query provided in the `:query"` parameter.
  + `nil` or no value provided: Same as `query` if a value is provided for the `:query` keyword parameter, otherwise same as `always`.

#### `define-grant`
An access-grant gives usage rights for a `graph-specification` to an `allowed-group` and can be created using the `define-grant` macro.

```lisp
grant (right &key to-graph for-allowed-group to for scopes)
```

Parameters:
- *`right`* A list of rights that is to be granted, currently only `read` and `write` are supported.

Keyword parameters:
- *`:to-graph`* The names of one or more a previously defined graph-specifications. If multiple names are provided they must be surrounded by brackets: `(someName anotherName)`.
- *`:for-allowed-group`* The names of one or more previously defined groups, each name quoted as a string. If multiple names are provided they must be surrounded by brackets: `("someGroup" "anotherGroup")`.
- *`:to`* Alias for `:to-graph`.
- *`:for`* Alias for `:for-allowed-group`.
- *`:scopes`* A list of URIs identifying the scopes in which this grant can be used. (default: `'(_)`)

**NOTE**: if values for both keyword parameters `:to-graph` and `:to` are provided these values are merged into a single list. Same for `:for-allowed-group` and `:for`.

#### `define-prefixes`
The `define-prefixes` macro allows to map prefixes to their corresponding expansion, allowing to use CURIEs in the configuration.

```lisp
define-prefixes (&body body)
```

The *`body`* has to be a sequence of keyword/value pairs, each pair of the form`:LABEL "EXPANSION"` where:

- Each keyword `:LABEL` MUST be preceded by a colon ':' and MAY NOT include a trailing colon.
- Each value `"EXPANSION"` MUST be a string and be surrounded by double quotes.
- Each prefix MUST be defined **before** its first use in the configuration file.
- It is allowed to have multiple `define-prefixes` in a single configuration file.


- **NOTE**: These prefixes **cannot** be used in SPARQL query strings such as those provided in `supply-allowed-group`. Such queries prefixes still need to specify their own prefixes using the `PREFIX` keyword.

The following concrete example snippet defines three prefixes.

```lisp
(in-package :acl)
(define-prefixes
  :foaf "http://xmlns.com/foaf/0.1/"
  :adms "http://www.w3.org/ns/adms#"
  :dcat "http://www.w3.org/ns/dcat#")
```

### Environment variables
- *`LISP_DYNAMIC_SPACE_SIZE`* Set the size (in megabytes) of the dynamic space reserved on startup by [sbcl](https://www.sbcl.org/manual/#Runtime-Options-1). (Default: `4096`)

### Configurable variables
The following sections list, per package, the available variables that can be (indirectly) configured.

#### acl
- *`*access-specifications*`* List of all known access groups. Do **not** edit directly but use the `acl:supply-allowed-group` macro to define groups. (default: `nil`)
- *`*graphs*`* List all all known graph-specification instances. Do **not** edit directly but use the `acl:define-graph` macro to add graph-specifications. (default: `nil`)
- *`*rights*`* List of all known grant instances connecting access-specification to graph. Do **not** edit directly but use the `acl:grant` macro to define grants. (default: `nil`)

#### client
- *`*backend*`* The SPARQL endpoint(s) to talk to, allowed values are either a single string or a list of strings. Over time this variable will be deprecated in favor of using `*backends*`. (default: `"http://triplestore:8890/sparql"`)
- *`*backends*`* A list of objects representing SPARQL endpoint(s) to talk to. The contained objects should be created using the `acl::make-sparql-endpoint` function with a URL string as argument. If not explicitly set, this variable is populated based on the value of `*backend*`. (default: `nil`)
- *`*max-concurrent-connections*`* The maximum amount of concurrent queries sent to an individual backend. (default: `8`)

- *`*log-sparql-query-roundtrip*`* If set to non-nil, log both the outgoing query sent to and response received from the backend to the standard output. (default: `nil`)
- *`*log-failing-query-tries*`* If set to non-nil, log queries which fail in exponential backoff retry to the standard output. (default: `t`)
- *`*log-failing-query-tries-with-condition*`* If set to non-nil, log the condition for queries which fail in exponential backoff retry to the standard output. (default: `t`)
- *`*log-batch-mapping*`* If set to non-nil, warn on processes which want to execute batch mapping. Note, batch mapping is **not yet implemented** and will process as one big query. (default: `nil`)

- *`*max-query-time-for-retries*`* This is the maximum amount of time (in seconds) to wait until retrying to send a query. (default: `10`)
- *`*max-query-time-for-retries-in-followup-queries*`* This is the maximum amount of time (in seconds) to wait until retrying to send the query once we've already sent the first query. (default: `50`)
- *`*acquire-db-semaphore-timeout*`* Amount of time (in seconds) to wait to acquire a semaphore for a SPARQL endpoint. If set to `nil`, wait forever. (default: `55`)

#### server
- *`*log-incoming-requests-p*`* If set to non-nil, log incoming requests and access rights for them to the standard output. (default: `nil`)

#### prefix
- *`*prefixes*`* List of known prefixes with their expansions. Do **not** edit directly, but use the `prefix:define-prefixes` macro to add entries. (default: `'(:skos "http://www.w3.org/2004/02/skos/core#" :schema "http://schema.org/" :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#)`)

- *`*uri-protocol-check-on-prefix-expansion*`* If set to non-nil, only expand CURIEs whose prefix expands to a URI for a protocol in `*uri-protocol-accept-list-for-prefix-expansion*` and signal an error upon encountering URIs with other protocols. If `nil`, expand any CURIE irrelevant of the protocol. (default: `t`)
- *`*uri-protocol-accept-list-for-prefix-expansion*`* A list of protocols that is accepted during the prefix expansion. (default: `'("http:" "https:" "mailto:" "ftp:" "ftps:" "share:)"`)

#### delta-messenger
- *`*delta-handlers*`* List of handlers for the delta messages. Do **not** edit directly but use the `delta-messenger:add-delta-messenger` function to add delta handlers. (default: `nil`)
- *`*log-delta-messenger-message-bus-processing*`* If set to non-nil, log when the delta messenger runs and for what to the standard output. (default `nil`)

- *`*max-sleep-on-idle-bus*`* The maximum amount of seconds to sleep until the bus is considered idle. This is a safety setting that would cover an erroneous semaphore implementation and can be disable by setting it to `nil`. (default: `60`)
- *`*message-bus-consumer*`* Consumer function to be called for each message on the message bus. (default: `delta-messenger::execute-scheduled-remote-delta-message`)

#### type-cache
- *`*uri-graph-user-type-providers*`* A list of functions that can calculate the types for a list of combined graph and URI. Do **not** set directly, but use the `type-cache::add-type-for-prefix` function to add types, this constructs the appropriate functions automatically. (default: `nil`)
- *`*debug-prefix-functions*`* If set to non-nil, emit debugging information for prefix type functions to the standard output. (default: `nil`)

#### support
- *`*string-max-size*`* Maximum size of a string before it gets converted to a file. (default: `4096`)
- *`*file-abbreviation-uri-prefix*`* Prefix for the URIs which will contain the abbreviation of a string. (default `http://services.redpencil.io/sparql-parser/abbreviations/`)
- *`*sha-file-directory*`* The directory where string-files will be stored, should be a mounted volume. (default `/data/strings/`)

#### administration
- *`*long-db-strings-to-move-per-batch*`* How many long strings in the DB are moved to files per batch. Used to batch the operations in `administration:update-database-long-strings-to-string-files`. (default: `10`)

#### quad-transformations
- *`*user-quad-transform-functions*`* List of quad transformation functions to try in the order in which they should be applied. These functions can be used to transform quads in insert or delete queries, for example to transform a single quad to multiple ones or vice versa. (default: `nil`)

#### handle-update-unit
- *`*max-query-size-heuristic*`* Heuristic indicating roughly how many characters the body of quads may be in a single query. Note, current implementation will try to query even if over this size. (default: `8000`)
- *`*max-quads-per-query-heuristic*`* Heuristic indicating roughly how many quads could be in a single query for insert or query. (default: `100`)

## Existing configurations
The following projects are currently using this service as a replacement of
`mu-authorization`, either fully or in a limited capacity (e.g. only on the
development or testing server). We link their configuration files to provide
a reference point for others trying out this service.

- [app-lokaal-mandatenbeheer](https://github.com/lblod/app-lokaal-mandatenbeheer/blob/master/config/cl-authorization/config.lisp)
- [app-kaleidos](https://github.com/kanselarij-vlaanderen/app-kaleidos/blob/development/config/new-authorization/config.lisp)
- [app-rollvolet](https://github.com/rollvolet/app-crm/blob/feature/next-mu-auth/config/cl-authorization/config.lisp)
