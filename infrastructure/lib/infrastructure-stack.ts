import * as cdk from "aws-cdk-lib";
import { Construct } from "constructs";
import * as s3 from "aws-cdk-lib/aws-s3";
import * as cloudfront from "aws-cdk-lib/aws-cloudfront";
import * as cloudfront_origins from "aws-cdk-lib/aws-cloudfront-origins";
import * as certificatemanager from "aws-cdk-lib/aws-certificatemanager";
import * as ec2 from "aws-cdk-lib/aws-ec2";

export class InfrastructureStack extends cdk.Stack {
    constructor(scope: Construct, id: string, props?: cdk.StackProps) {
        super(scope, id, props);

        const websiteBucket = new s3.Bucket(this, "WebsiteBucket");

        const certificate = certificatemanager.Certificate.fromCertificateArn(
            this,
            "Certificate",
            "arn:aws:acm:us-east-1:533267266440:certificate/801c9745-2fd1-4737-8e6c-3b341a3934e2"
        );

        const cloudfrontBehavior = {
            origin: cloudfront_origins.S3BucketOrigin.withOriginAccessControl(
                websiteBucket,
                { originAccessLevels: [cloudfront.AccessLevel.READ] }
            ),
            viewerProtocolPolicy:
                cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
        };

        const distribution = new cloudfront.Distribution(this, "Distribution", {
            defaultRootObject: "index.html",
            defaultBehavior: cloudfrontBehavior,
            additionalBehaviors: {
                "/*": cloudfrontBehavior,
            },
            errorResponses: [
                {
                    httpStatus: 403,
                    responseHttpStatus: 200,
                    responsePagePath: "/index.html",
                    ttl: cdk.Duration.minutes(5),
                },
                {
                    httpStatus: 404,
                    responseHttpStatus: 200,
                    responsePagePath: "/index.html",
                    ttl: cdk.Duration.minutes(5),
                },
            ],
            certificate: certificate,
            domainNames: [
                "*.waroftheringcommunity.net",
                "waroftheringcommunity.net",
            ],
        });

        const vpc = new ec2.Vpc(this, "VPC", { maxAzs: 2 });

        const securityGroup = new ec2.SecurityGroup(
            this,
            "ServerSecurityGroup",
            { vpc, allowAllOutbound: true }
        );

        securityGroup.addIngressRule(ec2.Peer.anyIpv4(), ec2.Port.tcp(80));
        securityGroup.addIngressRule(ec2.Peer.anyIpv4(), ec2.Port.tcp(443));

        const ami = ec2.MachineImage.latestAmazonLinux2();

        const instance = new ec2.Instance(this, "ServerInstance", {
            instanceType: ec2.InstanceType.of(
                ec2.InstanceClass.T3,
                ec2.InstanceSize.MICRO
            ),
            machineImage: ami,
            vpc,
            securityGroup,
        });

        const elasticIp = new ec2.CfnEIP(this, "ElasticIP");
        new ec2.CfnEIPAssociation(this, "EIPAssociation", {
            allocationId: elasticIp.attrAllocationId,
            instanceId: instance.instanceId,
        });

        new cdk.CfnOutput(this, "InstancePublicIP", {
            value: instance.instancePublicIp,
        });

        new cdk.CfnOutput(this, "DistributionDomainName", {
            value: distribution.distributionDomainName,
        });
    }
}
