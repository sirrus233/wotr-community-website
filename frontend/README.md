# Frontend (React + TypeScript)

Single-page app built with React, MUI Joy, and Webpack. The bundle is deployed to S3 and served via CloudFront.

## Prerequisites

- Latest Node and npm.
- Recommended editor extensions:
  - Prettier (format-on-save)
  - ESLint/TypeScript language features (bundled with VS Code)

## Install dependencies

```bash
cd frontend
npm install
```

Dependencies are pinned via `package-lock.json`. Re-run `npm install` whenever `package.json` changes.

## Running locally

```bash
npm start
```

- Starts `webpack-dev-server` on `http://localhost:3000`.
- Requests to the API are pointed at `http://localhost:8080` in dev mode (see `src/env/index.ts`). Production builds point at `https://api.waroftheringcommunity.net:8080`.
- `webpack-dev-server` automatically opens the site in your browser and supports client-side routing via `historyApiFallback`.
- New changes should be picked up immediately on save via hot reloading.

## Building for production

```bash
npm run build
```

- Outputs the optimized bundle to `dist/`.
- The `env` webpack alias swaps to `src/env/prod.ts` so that `API_BASE_URL` uses the prod endpoint.

## Deploying the static site

```bash
bin/deploy.sh
```

The script installs dependencies, builds the bundle, syncs `dist/` to the website S3 bucket (`infrastructurestack-websitebucket…`), and issues a `create-invalidation` request to the CloudFront distribution `E3885P1W9L1S68`. Ensure the AWS CLI `wotrcommunity` profile is configured and that you have permission to write to S3 and CloudFront before running it.
