import * as cdk from "aws-cdk-lib";
import { Construct } from "constructs";
import * as s3 from "aws-cdk-lib/aws-s3";
import * as cloudfront from "aws-cdk-lib/aws-cloudfront";
import * as cloudfront_origins from "aws-cdk-lib/aws-cloudfront-origins";
import * as certificatemanager from "aws-cdk-lib/aws-certificatemanager";

export class InfrastructureStack extends cdk.Stack {
    constructor(scope: Construct, id: string, props?: cdk.StackProps) {
        super(scope, id, props);

        const originAccessIdentity = new cloudfront.OriginAccessIdentity(
            this,
            "OriginAccessIdentity"
        );

        const bucket = new s3.Bucket(this, "Bucket");
        bucket.grantRead(originAccessIdentity);

        const certificate = certificatemanager.Certificate.fromCertificateArn(
            this,
            "Certificate",
            "arn:aws:acm:us-east-1:533267266440:certificate/801c9745-2fd1-4737-8e6c-3b341a3934e2"
        );

        const distribution = new cloudfront.Distribution(this, "Distribution", {
            defaultRootObject: "index.html",
            defaultBehavior: {
                origin: new cloudfront_origins.S3Origin(bucket, {
                    originAccessIdentity,
                }),
            },
            certificate: certificate,
            domainNames: [
                "*.waroftheringcommunity.net",
                "waroftheringcommunity.net",
            ],
        });

        new cdk.CfnOutput(this, "DistributionDomainName", {
            value: distribution.distributionDomainName,
        });
    }
}
