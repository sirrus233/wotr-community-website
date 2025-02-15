"""Migrate all game log files from Google Drive to S3."""

import csv
import re
import sys
import time
from datetime import UTC, datetime
from http import HTTPStatus
from pathlib import Path

import boto3
from mypy_boto3_s3.client import S3Client
from requests import Session


def to_s3_key(timestamp: str, free_player: str, shadow_player: str) -> str:
    """Convert game data to an S3 key.

    Constructs an S3 object key in the format:
      YYYY/MM/DD/YYYY-MM-DD_HHMMSS_FP_{free_player}_SP_{shadow_player}.log
    The input timestamp_str is assumed to be in the format:
      day/month/year hour:minute:second (e.g., 31/12/2024 23:59:59).
    """
    dt = datetime.strptime(timestamp, "%d/%m/%Y %H:%M:%S").astimezone(UTC)
    formatted_path = dt.strftime("%Y/%m/%d/")
    formatted_timestamp = dt.strftime("%Y-%m-%d_%H%M%S")
    formatted_filename = (
        f"{formatted_timestamp}_FP_{free_player}_SP_{shadow_player}.log"
    )
    return formatted_path + formatted_filename


def extract_file_id(url: str) -> str | None:
    """Extract the file id from a Google Drive URL.

    Supports URLs of the form:
      https://drive.google.com/file/d/FILE_ID/view?usp=sharing
    or
      https://drive.google.com/open?id=FILE_ID
    """
    match = re.search(r"/d/([a-zA-Z0-9_-]+)", url)
    if match:
        return match.group(1)
    match = re.search(r"id=([a-zA-Z0-9_-]+)", url)
    if match:
        return match.group(1)
    return None


def download_from_google_drive(session: Session, url: str) -> bytes | None:
    """Download the file at the given Google Drive URL.

    Returns the binary content, or None on failure.
    """
    file_id = extract_file_id(url)
    if not file_id:
        print("Unable to extract file id from URL:", url)
        return None

    base_url = "https://drive.google.com/uc?export=download"
    response = session.get(base_url, params={"id": file_id}, stream=True)

    if response.status_code != HTTPStatus.OK:
        print("Download failed with status:", response.status_code, "for URL:", url)
        return None

    return response.content


def upload_to_s3(s3_client: S3Client, bucket_name: str, key: str, data: bytes) -> None:
    """Upload the data to S3 under the provided bucket and key."""
    try:
        s3_client.put_object(Bucket=bucket_name, Key=key, Body=data)
    except (
        s3_client.exceptions.InvalidRequest,
        s3_client.exceptions.InvalidWriteOffset,
        s3_client.exceptions.TooManyParts,
        s3_client.exceptions.EncryptionTypeMismatch,
    ) as e:
        print(f"Failed to upload {key} to S3: {e}")


def process_csv(csv_file_path: Path, bucket_name: str, region: str) -> None:
    """Process the CSV file line-by-line.

    For each row that has a Google Drive URL, download the file and upload it to S3.
    """
    s3_client = boto3.client("s3", region_name=region)  # type: ignore[reportUnknown]
    session = Session()
    processed_count = 0
    limit = 10000

    with Path.open(csv_file_path, newline="", encoding="utf-8") as csv_file:
        reader = csv.reader(csv_file)
        for row in reader:
            game_id = row[0]
            timestamp = row[2]
            winning_side = row[49]
            winner = row[4]
            loser = row[5]
            drive_url = None if row[35] == "" else row[35]

            if winning_side not in ["FP", "SP"]:
                print("Invalid winning side in CSV row for game ID:", game_id)
                continue

            free_player = winner if winning_side == "FP" else loser
            shadow_player = winner if winning_side == "SP" else loser

            if not (timestamp and free_player and shadow_player):
                print("Missing required fields in CSV row for game ID:", game_id)
                continue

            s3_key = to_s3_key(timestamp, free_player, shadow_player)
            print(f"Uploading to s3://{bucket_name}/{s3_key}")

            if drive_url:
                file_data = download_from_google_drive(session, drive_url)

                if file_data is None:
                    print(f"Skipping row due to download failure for URL: {drive_url}")
                    continue

                upload_to_s3(s3_client, bucket_name, s3_key, file_data)
                processed_count += 1

            if processed_count >= limit:
                time.sleep(60)
                processed_count = 0


if __name__ == "__main__":
    expected_arg_count = 4
    if len(sys.argv) < expected_arg_count:
        print("Usage: python migration.py <csv_file_path> <bucket_name> <region>")
        sys.exit(1)
    csv_file_path = Path(sys.argv[1])
    bucket_name = sys.argv[2]
    region = sys.argv[3]
    process_csv(csv_file_path, bucket_name, region)
