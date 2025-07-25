# Run

- `cd` into this directory
- `cabal update`
- `cabal run server -- dev`

# Other setup

- In IDE
  - Open a workspace where `wotr-community-website.cabal` file is in the project root
  - Restart Haskell LSP Server

- Optionally, download a SQLite GUI tool:
  - All download methods: https://sqlitebrowser.org/dl/
  - With Homebrew: `brew install db-browser-for-sqlite`

# Troubleshooting

- `cabal clean`
- `ghcup list` and look for warnings about new versions available
- `ghcup set ghc <version number>` to switch GHC versions
