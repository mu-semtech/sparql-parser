# SPARQL Parser

A rewritten implementation of [`mu-authorization`](https://github.com/mu-semtech/mu-authorization) in Common Lisp.

> [!WARNING]
> This README is currently incomplete and configuring this service requires diving into the code and comparing with other existing configurations.
> We're working on writing a full configuration guide.

## Getting started
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

## Tutorials
### Specifying groups of users
sparql-parser does authentication based on user groups. We will later define which groups are allowed to perform which operations on which data. So first we need to define some user groups.
User groups are defined based on the result of a query involving the users session id. This can look as follows:
```lisp
(supply-allowed-group "super-mega-admins"
  :parameters ("session_group_id" "session_role")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

    SELECT ?session_group ?session_role WHERE {
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group_id;
                   ext:sessionRole ?session_role.
      FILTER( ?session_role = \"SuperMegaAdmin\" )
    }")
```
If this query returns a result, the user will belong to the `super-mega-admins` group. The value for `<SESSION_ID>` will be filled in automatically at runtime. The value of the variables listed in the `:parameters` argument will be joined using `/` and appended to the graph URI when it is accessed. This allows us to have a separate graph per user group.

### Specifying which triples are accessible from which graphs
For this we need a `define-graph` block. This will create a *graph spec* and looks as follows:
```lisp
(define-graph organization ("http://mu.semte.ch/graphs/organizations/")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" x> "ext:password"))
```
**NOTE**: Any prefixes such as `foaf` and `ext` need to be defined, see [Defining prefixes](#defining-prefixes)

The `define-graph` macro takes a unique identifier, the URI of the graph where the triples are stored, and one or more triple shapes.
The triple shapes have this form: `(<someResourceType> <operator> <somePredicate>)`. `<someResourceType>` and `<somePredicate>` must be a URI string (e.g. `"foaf:Person"`) or a `_` (indicating a wildcard). Triples that match these shapes will go to (or retrieved from) the specified graph (in the above example this is `http://mu.semte.ch/graphs/organizations/`). 
**Note**: different *graph specs* can specify the same graph URI.

These are all the possible operators:
- `T -> p`: Triples where the subject is of type `T` and the predicate is `p`.
- `T <- p`: Triples where the object is of type `T` and the predicate is `p`.
- `T x> p`: For triples where the subject is of type `T`, allow every predicate except for `p`.
- `T <x p`: For triples where the object is of type `T`, allow every predicate except for `p`.

In the above example this means the following:
- Matches all triples where the subject is of type `foaf:Person`.
- Matches all triples where the subject is of type `foaf:OnlineAccount` and where the predicate is not `ext:password`.

### Specifying which user groups have access to which graphs
Finally we need to specify which users are allowed to access which *graph spec*. This is done using the `grant` macro.
```lisp
(grant (read write)
  :to-graph (organization)
  :for-allowed-group "super-mega-admins")
```
This indicates that we allow the users in the `super-mega-admins` group to read and write tripes from and to the `http://mu.semte.ch/graphs/organizations/` graph, according to the triple restrictions in the `organization` *graph spec*.
The only allowed operation values are `read` and `write`.
`:to-graph` allows specifying multiple *graph specs*.
`:for-allowed-group` specifies which user group is allowed to execute the specified operations.


## Reference
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
### Existing configurations

The following projects are currently using this service as a replacement of
`mu-authorization`, either fully or in a limited capacity (e.g. only on the
development or testing server). We link their configuration files to provide
a reference point for others trying out this service.

- [app-lokaal-mandatenbeheer](https://github.com/lblod/app-lokaal-mandatenbeheer/blob/master/config/cl-authorization/config.lisp)
- [app-kaleidos](https://github.com/kanselarij-vlaanderen/app-kaleidos/blob/development/config/new-authorization/config.lisp)
- [app-rollvolet](https://github.com/rollvolet/app-crm/blob/feature/next-mu-auth/config/cl-authorization/config.lisp)
