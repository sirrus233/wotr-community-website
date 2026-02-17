# Backend (Haskell API)

Stateless Servant API that receives game reports, persists them in SQLite, and interacts with Amazon S3. The executable lives in `app/Server.hs`; shared code is in `src/`.

## Prerequisites

- [ghcup](https://www.haskell.org/ghcup/) with:
  - GHC `9.12.2` (`ghcup install ghc 9.12.2 && ghcup set ghc 9.12.2`)
  - cabal `3.14.2` (`ghcup install cabal 3.14.2`)
  - Haskell Language Server (for IDE support)
- `sqlite3` CLI plus (optionally) [DB Browser for SQLite](https://sqlitebrowser.org/dl/).
- Docker Desktop / `docker buildx` (release builds copy binaries out of a container).
- AWS CLI v2 configured with the `wotrcommunity` profile, IAM permissions for EC2/SSM/S3, and the [SSM Session Manager plugin](https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-install-plugin.html).

## Initial setup

```bash
cd backend
cabal update
cabal build
```

- Make sure your editor opens the workspace where `wotr-community-website.cabal` lives so HLS can start.

## Running locally

```bash
cabal run server -- dev migrate   # First run – applies DB migrations
cabal run server -- dev           # Subsequent runs
```

- The API listens on `http://localhost:8080`.
- CORS is configured to allow the frontend dev server on `http://localhost:3000`.
- Logs are written to `stdout` in dev mode and to `logs/app.log` in prod mode.

## Database & data files

- SQLite files are stored under `data/`:
  - `data/db.sqlite` – primary application data
  - `data/auth-db.sqlite` – OAuth and session data
- The server creates these files automatically; delete them if you need a clean slate.
- View/edit data with `sqlite3 data/db.sqlite` or DB Browser.
- `bin/backup_sqlite.sh` is used on the server by a systemd timer to gzip the DB and upload it to the `infrastructurestack-databasebackupbucket…` S3 bucket.

## Deployment workflow

1. Confirm SSM access: `ssh api.waroftheringcommunity.net` (tunnels through Session Manager—see SSH config below).
2. Ensure your working tree is clean, on `main`, and up to date.
3. Run `bin/deploy.sh`. The script:
   - Builds the `server` binary inside a linux/amd64 Docker builder.
   - Copies the binary out of the container to `/tmp/build-output/...`.
   - Stops the remote systemd service, uploads the binary via `scp`, and restarts the service.
4. Watch the logs on the host (`sudo journalctl -fu wotr-server`) if you need to verify the rollout.

`bin/config.sh` centralizes deployment constants such as the remote path, database bucket, and service name—update it before provisioning new infrastructure.

### SSH via AWS SSM

Add the EC2 host to `~/.ssh/config` so commands like `ssh api.waroftheringcommunity.net` work without exposing the instance publicly:

```
Host api.waroftheringcommunity.net
  User ec2-user
  IdentityFile <path_to_private_key>
  HostName <instance_id>          # e.g., i-0123456789abcdef0
  ProxyCommand sh -c "aws ssm start-session --profile wotrcommunity --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"
```

Replace `<instance_id>` with the current EC2 instance ID and `<path_to_private_key>` with your PEM file.

## Provisioning & maintenance scripts

- `bin/provision.sh` – Runs once per EC2 host to mount the attached volume, install `certbot`, configure systemd units (server, hourly DB backups, daily active-status cron), and upload `bin/backup_sqlite.sh`.
- `bin/backup_sqlite.sh` – Invoked by systemd to dump and gzip the SQLite database before pushing to S3.

Review these scripts before making changes as they assume an environment consistent with `bin/config.sh`.

## Troubleshooting

- `cabal clean && cabal build` fixes most build cache issues.
- `ghcup tui` highlights outdated GHC/cabal/HLS versions; install the recommended ones if builds fail due to mismatched toolchains.
- Database locked? Stop the server (`Ctrl+C`), make sure no other process is holding the SQLite file, and rerun with `cabal run server -- dev`.
- Deployment failures:
  - Re-run `ssh api.waroftheringcommunity.net` to confirm Session Manager connectivity.
  - If SSH fails, try `aws ssm --profile wotrcommunity start-session --target <instance_id>` directly to isolate SSM issues.
  - Ensure the instance ID inside `~/.ssh/config` matches the one currently in service.
