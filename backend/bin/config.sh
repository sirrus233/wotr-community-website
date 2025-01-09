#!/bin/bash

set -euo pipefail

export SSL_EMAIL="sirrus233@gmail.com"

export BUILD_DIR="/tmp/build-output"
export PROJECT_NAME="wotr-community-website"
export BIN_NAME="server"
export BIN_PATH="${BUILD_DIR}/build/x86_64-linux/ghc-9.10.1/${PROJECT_NAME}-0.1.0.0/x/${BIN_NAME}/build/${BIN_NAME}/${BIN_NAME}"
export SERVER_USER="ec2-user"
export SERVER_HOST="api.waroftheringcommunity.net"
export SERVICE_NAME="wotr-server"
export APP_DIR="/${SERVICE_NAME}"
export DATA_DIR="${APP_DIR}/data"
export DB_PATH="${DATA_DIR}/db.sqlite"
export DB_BACKUP_BUCKET=???
export SERVER_SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"
export BACKUP_SCRIPT_FILE="${APP_DIR}/bin/backup_sqlite.sh"
export BACKUP_SERVICE_FILE="/etc/systemd/system/backup-${SERVICE_NAME}.service"
export BACKUP_TIMER_FILE="/etc/systemd/system/backup-${SERVICE_NAME}.timer"
