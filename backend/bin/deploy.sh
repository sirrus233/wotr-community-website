#!/bin/sh

set -e

BIN_DIR=$(dirname "$0")
source "${BIN_DIR}/config.sh"

# Build the binary
echo "Building the Haskell binary..."
mkdir -p $BUILD_DIR
pushd "${BIN_DIR}/.." > /dev/null
cabal build --builddir=$BUILD_DIR
popd > /dev/null

if [[ ! -f "$BIN_PATH" ]]; then
  echo "Error: Binary not found at ${BIN_PATH}"
  exit 1
fi

echo "Binary built successfully."

# Upload to server
echo "Transferring binary to the server..."
scp "$BIN_PATH" "$SERVER_USER@$SERVER_HOST:$APP_DIR/$BIN_NAME"

# Restart service on server
echo "Restarting the server application..."
ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  set -e
  chmod +x "$APP_DIR/$BINARY_NAME"
  sudo systemctl restart $SERVICE_NAME
  sudo systemctl status $SERVICE_NAME --no-pager
EOF

echo "Deployment complete!"