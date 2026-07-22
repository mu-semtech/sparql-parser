#!/bin/bash
set -euo pipefail

# We clear /data so it appears to be mounted
rm -rf /data

# Drive the parent image's loader exactly like the running container:
#   SYSTEMS=SPARQL-PARSER  -> ql:quickload :sparql-parser (loads config, boots server)
#   EVAL=...               -> runs the admin call before swank/loop start
export SYSTEMS=SPARQL-PARSER/TESTS
export EVAL='(let ((result (asdf:test-system :sparql-parser))) (uiop:quit (if result 0 1)))'

exec /launch-sparql-parser.sh
