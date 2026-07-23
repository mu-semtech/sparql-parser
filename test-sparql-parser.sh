#!/usr/bin/env sh

sbcl --non-interactive --eval '(load "/app/sparql-parser.asd")' --eval '(ql:quickload :sparql-parser)' --eval '(let ((result (asdf:test-system :sparql-parser))) (uiop:quit (if result 0 1)))' --quit
