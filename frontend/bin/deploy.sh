#!/bin/sh

set -e

S3_BUCKET="infrastructurestack-websitebucket75c24d94-ok3sqluizbul"
CLOUDFRONT_DISTRIBUTION_ID="E3885P1W9L1S68"
BIN_DIR=$(dirname "$0")
BUILD_DIR="${BIN_DIR}/../dist"

echo "Starting frontend deployment..."

echo "Building the frontend..."
pushd "${BIN_DIR}/.." > /dev/null
npm install
npm run build
popd > /dev/null

echo "Uploading files to S3..."
aws --profile wotrcommunity s3 sync $BUILD_DIR s3://$S3_BUCKET --delete

echo "Invalidating CloudFront cache..."
aws --profile wotrcommunity cloudfront create-invalidation --distribution-id $CLOUDFRONT_DISTRIBUTION_ID --paths "/*"

echo "Frontend deployment complete!"
