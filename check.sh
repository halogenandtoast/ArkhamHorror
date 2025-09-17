#!/bin/bash

PORT=3002
CMD="/opt/arkham/bin/arkham-api"

# Check if the service is healthy
if ! curl -fs http://localhost:${PORT}/health > /dev/null; then
  echo "$(date) - Service on port ${PORT} is down. Restarting..."

  # Kill anything still running on that port
  fuser -k ${PORT}/tcp 2>/dev/null

  # Restart the service
  PORT=${PORT} ${CMD} &

  # Give it a few seconds to boot
  sleep 5

  # Check health again
  if curl -fs http://localhost:${PORT}/health > /dev/null; then
    echo "$(date) - Restart successful"
  else
    echo "$(date) - Restart failed"
  fi
else
  echo "$(date) - Service healthy"
fi
