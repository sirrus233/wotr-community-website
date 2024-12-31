#!/bin/sh

set -e

export SSL_EMAIL="sirrus233@gmail.com"

export BUILD_DIR="/tmp/out"
export PROJECT_NAME="wotr-community-website"
export BIN_NAME="server"
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     PLATFORM=x86_64-linux;;
    Darwin*)    PLATFORM=aarch64-osx;;
    *)          PLATFORM="UNKNOWN:${unameOut}"
esac
export BIN_PATH="${BUILD_DIR}/build/${PLATFORM}/ghc-9.10.1/${PROJECT_NAME}-0.1.0.0/x/${BIN_NAME}/build/${BIN_NAME}/${BIN_NAME}"

export SERVER_USER="ec2-user"
export SERVER_HOST="api.waroftheringcommunity.net"
export SERVICE_NAME="wotr-server"
export APP_DIR="/home/${SERVER_USER}/${SERVICE_NAME}"
export SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"
