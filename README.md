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
### Specifying which triples are accessible from which graphs
For this we need a `define-graph` block. This looks as follows:
```lisp
(define-graph organization ("http://mu.semte.ch/graphs/organizations/")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" x> "ext:password")
```
The `define-graph` macro takes a unique identifier, the URI of the graph where the triples are stored and one or more triple shapes.
The triple shapes have this form: `(<someResourceType> <operator> <somePredicate>)`. `<someResourceType>` and `<somePredicate>` must be a URI string (e.g. `"foaf:Person"`) or a `_` (indicating a wildcard). Triples that match these shapes will go to (or retrieved from) the specified graph (in the above example this is `http://mu.semte.ch/graphs/organizations/`).
These are all the possible operators:
- `T -> p`: Triples where the subject is of type `T` and the predicate is `p`.
- `T <- p`: Triples where the object is of type `T` and the predicate is `p`.
- `T x> p`: For triples where the subject is of type `T`, allow every predicate except for `p`.
- `T <x p`: For triples where the object is of type `T`, allow every predicate except for `p`.

In the above example this means the following:
- All triples where the subject is of type `foaf:Person` will be written to `http://mu.semte.ch/graphs/organizations/`.
- All triples where the subject is of type `foaf:OnlineAccount` and where the predicate is not `ext:password` will be written to `http://mu.semte.ch/graphs/organizations/`.
### Specifying which users have access to which graphs

## Reference
### Existing configurations

The following projects are currently using this service as a replacement of
`mu-authorization`, either fully or in a limited capacity (e.g. only on the
development or testing server). We link their configuration files to provide
a reference point for others trying out this service.

- [app-lokaal-mandatenbeheer](https://github.com/lblod/app-lokaal-mandatenbeheer/blob/master/config/cl-authorization/config.lisp)
- [app-kaleidos](https://github.com/kanselarij-vlaanderen/app-kaleidos/blob/development/config/new-authorization/config.lisp)
- [app-rollvolet](https://github.com/rollvolet/app-crm/blob/feature/next-mu-auth/config/cl-authorization/config.lisp)
