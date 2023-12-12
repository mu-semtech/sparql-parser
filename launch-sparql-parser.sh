#!/bin/bash

mkdir -p /config
cp /config/*.lisp /app/config/

exec /usr/src/startup.sh
