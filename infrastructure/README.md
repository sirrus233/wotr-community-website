# Infrastructure (AWS CDK)

The CDK app in this directory provisions everything that runs the site in AWS: the CloudFront distribution, website bucket, S3 buckets for uploads/backups, and the EC2 instance that hosts the Haskell API.

## Prerequisites

- Latest Node / npm.
- AWS CLI v2 configured with credentials in the `wotrcommunity` profile.
- CDK bootstrap stack deployed to the target account/region (run `npx cdk bootstrap aws://<account>/us-west-2 --profile wotrcommunity` once per account).
- Permissions to create IAM roles, EC2 instances, VPCs, CloudFront distributions, S3 buckets, ACM certificates, and Elastic IPs.

## Install dependencies

```bash
cd infrastructure
npm install
```

TypeScript is compiled to `dist/` when you run `npm run build`.

## Useful commands

| Command | Purpose |
| --- | --- |
| `npm run build` | Type-check and compile the CDK app to JavaScript. |
| `npx cdk synth --profile wotrcommunity` | Emit the CloudFormation template. |
| `npx cdk diff --profile wotrcommunity` | Compare the current template to the deployed stack. |
| `npx cdk deploy --profile wotrcommunity` | Deploy the stack (prompts for IAM/security-sensitive changes). |

Set the region explicitly if your default profile is in a different region: `CDK_DEFAULT_REGION=us-west-2 npx cdk deploy …`.

## Stack contents

`lib/infrastructure-stack.ts` defines a single `InfrastructureStack` that currently creates:

- A CloudFront distribution with ACM certificate `arn:aws:acm:us-east-1:533267266440:certificate/801c9745-…` for `waroftheringcommunity.net` and `*.waroftheringcommunity.net`.
- An S3 bucket for the static website plus an origin access control.
- S3 buckets for uploaded game logs and database backups, both granting write access to the EC2 instance.
- A VPC with public subnets, an EC2 instance, EBS volume for storage, and an Elastic IP.
- IAM role attachments for SSM + S3 write permissions.
- Security-group rules opening ports 80/443/8080 to the world.

Tweak any resource-specific constants (certificate ARN, domain names, bucket policies, instance size, availability zone, etc.) in `infrastructure-stack.ts` before redeploying to a new account.

## Deploying changes

1. Commit the application/backend changes that depend on new infrastructure so the diff stays reviewable.
2. Run `npm run build`.
3. Run `npx cdk diff --profile wotrcommunity --all` to see the impact.
4. Run `npx cdk deploy --profile wotrcommunity` to update the stack. This may take several minutes, especially if EC2 or CloudFront resources are replaced.
5. After the stack completes, update any hard-coded identifiers in `backend/bin/config.sh` or the frontend deploy script (S3 bucket names, distribution IDs) if CloudFormation generated new values.
