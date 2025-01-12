#!/bin/bash

set -euo pipefail

BIN_DIR=$(dirname "$0")
source "${BIN_DIR}/config.sh"

DEVICE_NAME=/dev/nvme1n1
MOUNT_POINT=${APP_DIR}
FILESYSTEM=xfs

ssh "$SERVER_USER@$SERVER_HOST" <<EOF
  set -e

  echo "Creating storage mount point at $MOUNT_POINT..."
  sudo mkdir -p $MOUNT_POINT

  if [ ! -e "$DEVICE_NAME" ]; then
    echo "Error: Device $DEVICE_NAME does not exist."
    exit 1
  fi

  if [ -z \$(sudo blkid -s TYPE -o value $DEVICE_NAME 2>/dev/null) ]; then
    echo "No filesystem found on $DEVICE_NAME. Formatting with $FILESYSTEM..."
    sudo mkfs -t $FILESYSTEM $DEVICE_NAME
  else
    echo "Filesystem already exists on $DEVICE_NAME."
  fi

  UUID=\$(sudo blkid -s UUID -o value ${DEVICE_NAME})
  if [ -z \$UUID ]; then
    echo "Error: Unable to retrieve UUID for $DEVICE_NAME."
    exit 1
  fi
  echo "UUID of $DEVICE_NAME is \$UUID"

  echo "Ensuring $DEVICE_NAME mounts at boot using UUID..."
  if ! grep -qs "UUID=\$UUID" /etc/fstab; then
    echo "Adding UUID=\$UUID to /etc/fstab..."
    echo "UUID=\$UUID $MOUNT_POINT $FILESYSTEM defaults,nofail 0 2" | sudo tee -a /etc/fstab
  else
    echo "/etc/fstab already contains an entry for UUID=\$UUID."
  fi

  echo "Mounting $DEVICE_NAME to $MOUNT_POINT..."
  if mount | grep -q "$DEVICE_NAME"; then
    echo "$DEVICE_NAME is already mounted. Skipping direct mount."
  else
    sudo mount -a
  fi

  mkdir -p ${APP_DIR}
  mkdir -p "${APP_DIR}/bin"
  sudo chown -R ${SERVER_USER}:${SERVER_USER} ${APP_DIR}

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
  
  echo "Generating server startup service file..."
  cat <<END_UNIT | sudo tee $SERVER_SERVICE_FILE
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

  echo "Enabling server startup service..."
  sudo systemctl daemon-reload
  sudo systemctl enable $SERVICE_NAME

  echo "Installing sqlite dependency..."
  sudo yum install -y sqlite

  echo "Generating database backup service file..."
  cat <<END_UNIT | sudo tee $BACKUP_SERVICE_FILE
[Unit]
Description=Backup the WotR SQLite database to S3

[Service]
Type=oneshot
ExecStart=${BACKUP_SCRIPT_FILE} ${DB_PATH} ${DB_BACKUP_BUCKET}
END_UNIT

  echo "Generating database backup timer file..."
  cat <<END_UNIT | sudo tee $BACKUP_TIMER_FILE
[Unit]
Description=Database backup timer

[Timer]
OnCalendar=*-*-* *:00:00
Persistent=true

[Install]
WantedBy=timers.target
END_UNIT

  echo "Staring backup service timer..."
  sudo systemctl daemon-reload
  sudo systemctl enable --now backup-${SERVICE_NAME}.timer
EOF

echo "Uploading database backup script..."
scp ${BIN_DIR}/backup_sqlite.sh ${SERVER_USER}@${SERVER_HOST}:${BACKUP_SCRIPT_FILE}

echo "Provisioning complete!"
