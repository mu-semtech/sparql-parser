#!/usr/bin/env sh

sbcl --non-interactive \
    --eval '(load "sparql-parser.asd")' \
    --eval '(ql:quickload :sparql-parser)' \
    --eval '(sb-thread:make-thread (lambda () (error "test error")))' \
    --eval '(sleep 1)'
