#!/bin/sh

set -e

BACKEND_DIR=$(dirname "$0")/..
source "${BACKEND_DIR}/bin/config.sh"

echo "Building the Haskell binary..."
mkdir -p "${BIN_PATH}"
docker build --tag wotr-server-build "${BACKEND_DIR}/."
docker create --name wotr-server-build wotr-server-build
docker cp "wotr-server-build:${BIN_PATH}" "${BIN_PATH}"
docker rm wotr-server-build

if [[ ! -f "${BIN_PATH}/${BIN_NAME}" ]]; then
  echo "Error: Binary not found at ${BIN_PATH}"
  exit 1
fi

echo "Binary built successfully."

echo "Stopping the server..."
ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  sudo systemctl stop $SERVICE_NAME
EOF

echo "Transferring new binary to the server..."
scp "$BIN_PATH/$BIN_NAME" "$SERVER_USER@$SERVER_HOST:$APP_DIR/$BIN_NAME"

echo "Restarting the server application..."
ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  set -e
  sudo systemctl restart $SERVICE_NAME
  sudo systemctl status $SERVICE_NAME --no-pager
EOF

echo "Deployment complete!"
