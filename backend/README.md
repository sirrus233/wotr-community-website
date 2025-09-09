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

- Set up for deployment

  - Install and configure AWS CLI
  - Add a `wotrcommunity` AWS profile with IAM credentials
  - Get a private key allowing SSH into the EC2 instance
  - [Install the Session Manager plugin for AWS CLI](https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-install-plugin.html)
  - Add the following to `~/.ssh/config`:
    ```
    Host api.waroftheringcommunity.net
      User ec2-user
      IdentityFile <private_key_file_path>
      HostName <instance_id>
      ProxyCommand sh -c "aws ssm start-session --profile wotrcommunity --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"
    ```
    Where `<instance_id>` can be found in the EC2 console, and `<private_key_file_path>` is where you saved your private key

# Deploy

- Check that your `aws ssm` connection will succeed with `ssh api.waroftheringcommunity.net`, then exit the connection
- `git checkout main`
- `git pull`
- Check that you have no local changes
- `bin/deploy.sh`

# Troubleshooting

## Issues running local server

- `cabal clean`
- `ghcup list` and look for warnings about new versions available
- `ghcup set ghc <version number>` to switch GHC versions

## Issues deploying

- Check that your `aws ssm` connection will succeed with `ssh api.waroftheringcommunity.net`
- If that fails, `aws ssm --profile wotrcommunity start-session --target <instance_id>` to confirm your `aws ssm` configuration in isolation from your `~/.ssh/config` file
- Check that the instance ID in `~/.ssh/config` is up to date
