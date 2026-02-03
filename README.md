# SPARQL Parser

A rewritten implementation of [`mu-authorization`](https://github.com/mu-semtech/mu-authorization) in Common Lisp.

> [!WARNING]
> This README is currently incomplete and configuring this service requires diving into the code and comparing with other existing configurations.
> We're working on writing a full configuration guide.

## Tutorials
### How to add the sparql-server to your application
Add the service to your `docker-compose.yml`:
```yaml
services:
  database:
    image: semtech/sparql-parser:0.0.8
    volumes:
      - ./config/authorization:/config
      - ./data/authorization:/data
```

Next, add the following contents to the config file mounted in `./config/authorization/config.lisp`.
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

;;;;;;;;;;;;;;;;;
;;; access rights
(in-package :acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")

(type-cache::add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session")

(define-graph public ("http://mu.semte.ch/graphs/public")
  (_ -> _))

(supply-allowed-group "public")

(grant (read write)
  :to-graph (public)
  :for-allowed-group "public")
```

It basically configures read/write access for everyone for all data on the `http://mu.semte.ch/graphs/public` graph.

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

While it is rather common to define group membership based on roles, sparql-parser is not limited to this and allows arbitrary queries to be specified. It depends on your application's data model which queries make sense. Note, in the above examples the actual matches returned by the queries are not used, [TODO: link to guide] shows how you can use these matches to simplify access control policies in some situations.

### Define which triples are accessible for a graph
Typically you want to explicitly specify which (kind of) triples within a graph an access control rule can be applied to. In sparql-parser such information is captured by *graph-specifications* which you create using the `define-graph` macro. For readability the code snippets use CURIEs as explained in the guide on TODO: link to prefix guide

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

### Defining prefixes
In order to use the CURIE (Compact URI) form (e.g. `foaf:name`) we need to define the prefixes first. This is done as follows:
```lisp
(define-prefixes
  :adms "http://www.w3.org/ns/adms#"
  :cal "http://www.w3.org/2002/12/cal/ical#"
  :cogs "http://vocab.deri.ie/cogs#"
  :dcat "http://www.w3.org/ns/dcat#"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :eli "http://data.europa.eu/eli/ontology#")
```
**NOTE**: This does not affect prefixes that can be used in sparql query strings used in this config. They still need to be specified using the `PREFIX` keyword.

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
- *`:scopes`* TODO

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
