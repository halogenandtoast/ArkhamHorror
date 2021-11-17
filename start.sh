#!/bin/bash

sed -i -e 's/$PORT/'"$PORT"'/g' /opt/arkham/src/backend/prod.nginxconf
PORT=3002 /opt/arkham/bin/arkham-api &
nginx -c /opt/arkham/src/backend/prod.nginxconf -g 'daemon off;'
