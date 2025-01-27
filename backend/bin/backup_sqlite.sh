#!/bin/bash

set -euo pipefail

DB_PATH=$1
DB_BACKUP_BUCKET=$2
YEAR=$(date +"%Y")
MONTH=$(date +"%m")
DAY=$(date +"%d")
TIMESTAMP=$(date +"%Y-%m-%dT%H:%M:%SZ")
TMP_BACKUP="/tmp/db-backup-$TIMESTAMP.sqlite"

sqlite3 "$DB_PATH" ".backup '$TMP_BACKUP'"
gzip "$TMP_BACKUP"
TMP_BACKUP_ZIP="${TMP_BACKUP}.gz"
aws s3 cp "$TMP_BACKUP_ZIP" "s3://${DB_BACKUP_BUCKET}/${YEAR}/${MONTH}/${DAY}/${TIMESTAMP}.sqlite.gz"
rm "$TMP_BACKUP_ZIP"
