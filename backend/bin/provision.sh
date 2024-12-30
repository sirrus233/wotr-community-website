#!/bin/sh

set -e

BIN_DIR=$(dirname "$0")
source "${BIN_DIR}/config.sh"
  
ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  set -e
  
  mkdir -p wotr-server
  
  echo "Acquiring SSL certificate..."
  sudo yum install -y certbot
  sudo certbot certonly --standalone -d $SERVER_HOST -m $SSL_EMAIL --agree-tos --non-interactive
  sudo systemctl enable --now certbot-renew.timer
  
  echo "Setting system permissions..."
  sudo groupadd -f ssl-cert
  sudo usermod -a -G ssl-cert ec2-user
  sudo chgrp -R ssl-cert "/etc/letsencrypt/live"
  sudo chgrp -R ssl-cert "/etc/letsencrypt/archive"
  sudo chmod -R 750 "/etc/letsencrypt/live"
  sudo chmod -R 750 "/etc/letsencrypt/archive"
  
  echo "Generating service file..."
  cat <<END_UNIT | sudo tee $SERVICE_FILE
[Unit]
Description=War of the Ring API Server
After=network.target

[Service]
WorkingDirectory=${APP_DIR}
ExecStart=${APP_DIR}/${BIN_NAME}
Restart=always
User=ec2-user
Group=ec2-user
Environment="PORT=8080"
Environment="SSL_CERT_PATH=/etc/letsencrypt/live/${SERVER_HOST}/fullchain.pem"
Environment="SSL_KEY_PATH=/etc/letsencrypt/live/${SERVER_HOST}/privkey.pem"

[Install]
WantedBy=multi-user.target
END_UNIT

  echo "Enabling service..."
  sudo systemctl daemon-reload
  sudo systemctl enable $SERVICE_NAME
  
  echo "Provisioning complete!"
EOF
