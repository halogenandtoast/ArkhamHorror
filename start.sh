#!/bin/bash

cron -f &
PORT=3002 /opt/arkham/bin/arkham-api &
nginx -c /opt/arkham/src/backend/prod.nginxconf -g 'daemon off;'
