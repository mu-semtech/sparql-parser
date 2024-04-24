# SPARQL Parser

A rewritten implementation of [`mu-authorization`](https://github.com/mu-semtech/mu-authorization) in Common Lisp.

> [!WARNING]  
> This README is currently incomplete and configuring this service requires diving into the code and comparing with other existing configurations.
> We're working on writing a full configuration guide.

## Existing configurations

The following projects are currently using this service as a replacement of
`mu-authorization`, either fully or in a limited capacity (e.g. only on the
development or testing server). We link their configuration files to provide
a reference point for others trying out this service.

- [app-lokaal-mandatenbeheer](https://github.com/lblod/app-lokaal-mandatenbeheer/blob/master/config/cl-authorization/config.lisp)
- [app-kaleidos](https://github.com/kanselarij-vlaanderen/app-kaleidos/blob/development/config/new-authorization/config.lisp)
