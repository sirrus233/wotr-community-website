#!/bin/bash

set -euo pipefail

BACKEND_DIR=$(dirname "$0")/..
source "${BACKEND_DIR}/bin/config.sh"

echo "Rebuilding database locally..."
rm -r ${BACKEND_DIR}/data
pushd ${BACKEND_DIR} > /dev/null
cabal run -- migration
popd > /dev/null

echo "Stopping server..."
ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  sudo systemctl stop $SERVICE_NAME
EOF

echo "Deleting old database..."
ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  rm -f ${DATA_DIR}/*
EOF

echo "Uploading new database..."
scp ${BACKEND_DIR}/data/* ${SERVER_USER}@${SERVER_HOST}:${DATA_DIR}

echo "Starting server..."
ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  sudo systemctl start $SERVICE_NAME
EOF

echo "Migration complete!"
