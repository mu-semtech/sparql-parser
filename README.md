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

## Reference
### Existing configurations

The following projects are currently using this service as a replacement of
`mu-authorization`, either fully or in a limited capacity (e.g. only on the
development or testing server). We link their configuration files to provide
a reference point for others trying out this service.

- [app-lokaal-mandatenbeheer](https://github.com/lblod/app-lokaal-mandatenbeheer/blob/master/config/cl-authorization/config.lisp)
- [app-kaleidos](https://github.com/kanselarij-vlaanderen/app-kaleidos/blob/development/config/new-authorization/config.lisp)
- [app-rollvolet](https://github.com/rollvolet/app-crm/blob/feature/next-mu-auth/config/cl-authorization/config.lisp)
