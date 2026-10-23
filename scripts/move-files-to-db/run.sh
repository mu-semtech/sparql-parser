#!/bin/bash
set -euo pipefail

mkdir -p /project/config/authorization /project/data/authorization/strings
rm -rf /config /data
ln -s /project/config/authorization /config
ln -s /project/data/authorization /data

export SYSTEMS=SPARQL-PARSER
export EVAL='(progn (administration:update-database-string-files :database-p nil :files-p t) (sb-ext:exit :code 0))'

exec /launch-sparql-parser.sh
