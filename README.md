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
### Specifying groups of users
sparql-parser does authentication based on user groups. We will later define which groups are allowed to perform which operations on which data. So first we need to define some user groups.
User groups are defined based on the result of a query involving the user's session id. This can look as follows:
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

The `define-graph` macro takes:
- A unique identifier (*here `organization`*)
- The URI of the graph where the triples are stored (*here `http://mu.semte.ch/graphs/organizations/`*)
- One or more triple shapes (*here `("foaf:Person" -> _)` and `("foaf:OnlineAccount" x> "ext:password")`*).

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
Finally we need to specify which *user groups* are allowed to access which *graph spec*. This is done using the `grant` macro.
```lisp
(grant (read write)
  :to-graph (organization)
  :for-allowed-group "super-mega-admins")
```
This indicates that we allow the users in the `super-mega-admins` group to read and write tripes from and to the `http://mu.semte.ch/graphs/organizations/` graph, according to the triple restrictions in the `organization` *graph spec*.

The only allowed operation values are `read` and `write`.

`:to-graph` allows specifying multiple *graph specs*.

`:for-allowed-group` specifies which user group is allowed to execute the specified operations.

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

## Existing configurations
The following projects are currently using this service as a replacement of
`mu-authorization`, either fully or in a limited capacity (e.g. only on the
development or testing server). We link their configuration files to provide
a reference point for others trying out this service.

- [app-lokaal-mandatenbeheer](https://github.com/lblod/app-lokaal-mandatenbeheer/blob/master/config/cl-authorization/config.lisp)
- [app-kaleidos](https://github.com/kanselarij-vlaanderen/app-kaleidos/blob/development/config/new-authorization/config.lisp)
- [app-rollvolet](https://github.com/rollvolet/app-crm/blob/feature/next-mu-auth/config/cl-authorization/config.lisp)
