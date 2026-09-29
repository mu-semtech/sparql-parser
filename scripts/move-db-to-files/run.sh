#!/bin/bash
set -euo pipefail

# The production launcher /launch-sparql-parser.sh expects the user config
# at /config and a mounted /data.
mkdir -p /project/config/authorization /project/data/authorization/strings
rm -rf /config /data
ln -s /project/config/authorization /config
ln -s /project/data/authorization /data

# Drive the parent image's loader exactly like the running container:
#   SYSTEMS=SPARQL-PARSER  -> ql:quickload :sparql-parser (loads config, boots server)
#   EVAL=...               -> runs the admin call before swank/loop start
export SYSTEMS=SPARQL-PARSER
export EVAL='(progn (administration:update-database-string-files :database-p t :files-p nil) (sb-ext:exit :code 0))'

exec /launch-sparql-parser.sh
