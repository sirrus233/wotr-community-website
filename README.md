# War of the Ring Community Website

Mono-repo containing every part of the War of the Ring Community site:

- `backend/` – Haskell (Servant) API that stores game reports in SQLite and uploads game logs to S3.
- `frontend/` – React + TypeScript single-page app served from CloudFront/S3.
- `infrastructure/` – AWS CDK stack that provisions EC2, networking, S3 buckets, and CloudFront.
- `scripts/` – Miscellaneous utilities such as the game-log migration helper.

Each directory has its own README with more details:

- [backend/README.md](backend/README.md)
- [frontend/README.md](frontend/README.md)
- [infrastructure/README.md](infrastructure/README.md)

## General prerequisites

- Latest Node.js and npm (install via [nvm](https://github.com/nvm-sh/nvm), [Volta](https://volta.sh/), or your system's package manager).
- Haskell toolchain via [ghcup](https://www.haskell.org/ghcup/) with GHC `9.12.2`, cabal `3.14.2`, and Haskell Language Server.
- `sqlite3` CLI plus an optional GUI such as [DB Browser for SQLite](https://sqlitebrowser.org/).
- Docker (for building backend release binaries) and AWS CLI v2 configured with the `wotrcommunity` profile+credentials, plus the AWS Session Manager plugin.

## Local development workflow

1. Run `cabal run server -- dev migrate` inside `backend/` the first time to create the SQLite databases, then `cabal run server -- dev` afterwards. The API listens on `http://localhost:8080`.
2. Run `npm start` inside `frontend/` to start the Webpack dev server on `http://localhost:3000`. API requests are sent to `http://localhost:8080` automatically in dev mode.
3. Open `http://localhost:3000` to interact with the app. The backend logs go to `stdout` by default.

## Deployment quick reference

- API: run `backend/bin/deploy.sh` after ensuring you can `ssh api.waroftheringcommunity.net` through SSM.
- Web app: run `frontend/bin/deploy.sh` to build and sync the static bundle to the website S3 bucket, then invalidate CloudFront.
- Infrastructure: run the CDK workflow from `infrastructure/` (`npm run build && npx cdk diff/deploy`) using the `wotrcommunity` AWS profile in `us-west-2`.

Always refer back to the per-project READMEs for the detailed steps, environment variables, and troubleshooting tips.
