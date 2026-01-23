#!/bin/bash

mkdir -p /config
cp /config/*.{lisp,nt} /app/config/

exec /usr/src/startup.sh
