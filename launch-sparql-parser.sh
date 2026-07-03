#!/bin/bash

mkdir -p /config
cp /config/*.{lisp,ttl} /app/config/

exec /usr/src/startup.sh
