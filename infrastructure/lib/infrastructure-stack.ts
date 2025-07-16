import * as cdk from "aws-cdk-lib";
import { Construct } from "constructs";
import * as s3 from "aws-cdk-lib/aws-s3";
import * as cloudfront from "aws-cdk-lib/aws-cloudfront";
import * as cloudfront_origins from "aws-cdk-lib/aws-cloudfront-origins";
import * as certificatemanager from "aws-cdk-lib/aws-certificatemanager";
import * as ec2 from "aws-cdk-lib/aws-ec2";
import * as iam from "aws-cdk-lib/aws-iam";

export class InfrastructureStack extends cdk.Stack {
    constructor(scope: Construct, id: string, props?: cdk.StackProps) {
        super(scope, id, props);

        const elasticIp = new ec2.CfnEIP(this, "ElasticIP");

        const gameReportBucket = new s3.Bucket(this, "GameReportBucket", {
            blockPublicAccess: new s3.BlockPublicAccess({
                blockPublicAcls: false,
                blockPublicPolicy: false,
                ignorePublicAcls: false,
                restrictPublicBuckets: false,
            }),
        });
        gameReportBucket.grantPublicAccess();

        const databaseBackupBucket = new s3.Bucket(
            this,
            "DatabaseBackupBucket"
        );

        this.website();
        this.server(elasticIp, [gameReportBucket, databaseBackupBucket]);
    }

    website() {
        const websiteBucket = new s3.Bucket(this, "WebsiteBucket");

        const certificate = certificatemanager.Certificate.fromCertificateArn(
            this,
            "Certificate",
            "arn:aws:acm:us-east-1:533267266440:certificate/801c9745-2fd1-4737-8e6c-3b341a3934e2"
        );

        const distribution = new cloudfront.Distribution(this, "Distribution", {
            defaultRootObject: "index.html",
            defaultBehavior: {
                origin: cloudfront_origins.S3BucketOrigin.withOriginAccessControl(
                    websiteBucket,
                    { originAccessLevels: [cloudfront.AccessLevel.READ] }
                ),
                viewerProtocolPolicy:
                    cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
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

        new cdk.CfnOutput(this, "DistributionDomainName", {
            value: distribution.distributionDomainName,
        });
    }

    server(elasticIp: ec2.CfnEIP, writeableBuckets: s3.Bucket[]) {
        const ami = ec2.MachineImage.latestAmazonLinux2023();

        const vpc = new ec2.Vpc(this, "VPC", {
            maxAzs: 2,
            subnetConfiguration: [
                {
                    cidrMask: 24,
                    name: "Public",
                    subnetType: ec2.SubnetType.PUBLIC,
                },
            ],
        });

        const role = new iam.Role(this, "ServerRole", {
            assumedBy: new iam.ServicePrincipal("ec2.amazonaws.com"),
        });

        role.addManagedPolicy(
            iam.ManagedPolicy.fromAwsManagedPolicyName(
                "AmazonSSMManagedInstanceCore"
            )
        );

        writeableBuckets.forEach((bucket) => bucket.grantPut(role));

        const instance = new ec2.Instance(this, "ServerInstance", {
            instanceType: ec2.InstanceType.of(
                ec2.InstanceClass.T3,
                ec2.InstanceSize.MICRO
            ),
            machineImage: ami,
            vpc,
            role,
            keyPair: ec2.KeyPair.fromKeyPairName(
                this,
                "ServerKey",
                "ServerKey"
            ),
        });

        const ebsVolume = new ec2.Volume(this, "ServerStorage", {
            availabilityZone: "us-west-2a",
            size: cdk.Size.gibibytes(8),
            volumeType: ec2.EbsDeviceVolumeType.GP3,
            removalPolicy: cdk.RemovalPolicy.RETAIN,
        });

        new ec2.CfnVolumeAttachment(this, "AttachServerStorage", {
            volumeId: ebsVolume.volumeId,
            instanceId: instance.instanceId,
            device: "/dev/sda2",
        });

        instance.connections.allowFromAnyIpv4(ec2.Port.tcp(80));
        instance.connections.allowFromAnyIpv4(ec2.Port.tcp(443));
        instance.connections.allowFromAnyIpv4(ec2.Port.tcp(8080));

        new ec2.CfnEIPAssociation(this, "EIPAssociation", {
            allocationId: elasticIp.attrAllocationId,
            instanceId: instance.instanceId,
        });

        new cdk.CfnOutput(this, "InstancePublicIP", {
            value: instance.instancePublicIp,
        });
    }
}
