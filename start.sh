#!/bin/bash

PORT=3002 /opt/arkham/bin/arkham-api &

# The custom-card MCP server, for agents writing cards on their owners' accounts.
#
# Loopback only: nginx owns :3000 and proxies /mcp here (see prod.nginxconf), so
# the credential arrives over the load balancer's TLS and never crosses a network
# in the clear. It holds no signing secret of its own -- it forwards the caller's
# Authorization header to arkham-api on :3002 and lets that decide who they are.
#
# Guarded on the file existing so an image built without mcp/ still boots, and
# backgrounded like the API so nginx stays the foreground process that keeps the
# container alive.
if [ -f /opt/arkham/mcp/arkham-cards/http_server.py ]; then
  # JWT_SECRET is deliberately removed from its environment. The app container has
  # it (it signs login tokens), and a process holding it can mint a token for any
  # user id -- which is precisely what a multi-tenant server must not be able to
  # do. http_server.py never reads it, and unsetting it means that if some future
  # change makes it try, it fails instead of quietly impersonating somebody.
  env -u JWT_SECRET \
  ARKHAM_API=http://localhost:3002 \
  ARKHAM_REFERENCE_DIR=/opt/arkham/mcp \
  MCP_HOST=127.0.0.1 \
  MCP_PORT="${MCP_PORT:-8420}" \
  MCP_PUBLIC_URL="${MCP_PUBLIC_URL:-https://arkhamhorror.app/mcp}" \
  MCP_REQUESTS_PER_MINUTE="${MCP_REQUESTS_PER_MINUTE:-240}" \
    python3 /opt/arkham/mcp/arkham-cards/http_server.py &
fi

nginx -c /opt/arkham/src/backend/prod.nginxconf -g 'daemon off;'
