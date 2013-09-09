{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Auto Scaling is a web service designed to automatically launch or terminate
-- Amazon Elastic Compute Cloud (Amazon EC2) instances based on user-defined
-- policies, schedules, and health checks.
--
-- This service is used in conjunction with Amazon CloudWatch and
-- Elastic Load Balancing services.
module Network.AWS.AutoScaling
   (
   -- * Actions
   -- ** CreateAutoScalingGroup
     CreateAutoScalingGroup               (..)

   -- ** CreateLaunchConfiguration
   , CreateLaunchConfiguration            (..)

   -- ** CreateOrUpdateTags
   , CreateOrUpdateTags                   (..)

   -- ** DeleteAutoScalingGroup
   , DeleteAutoScalingGroup               (..)

   -- ** DeleteLaunchConfiguration
   , DeleteLaunchConfiguration            (..)

   -- ** DeleteNotificationConfiguration
   , DeleteNotificationConfiguration      (..)

   -- ** DeletePolicy
   , DeletePolicy                         (..)

   -- ** DeleteScheduledAction
   , DeleteScheduledAction                (..)

   -- ** DeleteTags
   , DeleteTags                           (..)

   -- ** DescribeAdjustmentTypes
   , DescribeAdjustmentTypes              (..)

   -- ** DescribeAutoScalingGroups
   , DescribeAutoScalingGroups            (..)

   -- ** DescribeAutoScalingInstances
   , DescribeAutoScalingInstances         (..)

   -- ** DescribeAutoScalingNotificationTypes
   , DescribeAutoScalingNotificationTypes (..)

   -- ** DescribeLaunchConfigurations
   , DescribeLaunchConfigurations         (..)

   -- ** DescribeMetricCollectionTypes
   , DescribeMetricCollectionTypes        (..)

   -- ** DescribeNotificationConfigurations
   , DescribeNotificationConfigurations   (..)

   -- ** DescribePolicies
   , DescribePolicies                     (..)

   -- ** DescribeScalingActivities
   , DescribeScalingActivities            (..)

   -- ** DescribeScalingProcessTypes
   , DescribeScalingProcessTypes          (..)

   -- ** DescribeScheduledActions
   , DescribeScheduledActions             (..)

   -- ** DescribeTags
   , DescribeTags                         (..)

   -- ** DescribeTerminationPolicyTypes
   , DescribeTerminationPolicyTypes       (..)

   -- ** DisableMetricsCollection
   , DisableMetricsCollection             (..)

   -- ** EnableMetricsCollection
   , EnableMetricsCollection              (..)

   -- ** ExecutePolicy
   , ExecutePolicy                        (..)

   -- ** PutNotificationConfiguration
   , PutNotificationConfiguration         (..)

   -- ** PutScalingPolicy
   , PutScalingPolicy                     (..)

   -- ** PutScheduledUpdateGroupAction
   , PutScheduledUpdateGroupAction        (..)

   -- ** ResumeProcesses
   , ResumeProcesses                      (..)

   -- ** SetDesiredCapacity
   , SetDesiredCapacity                   (..)

   -- ** SetInstanceHealth
   , SetInstanceHealth                    (..)

   -- ** SuspendProcesses
   , SuspendProcesses                     (..)

   -- ** TerminateInstanceInAutoScalingGroup
   , TerminateInstanceInAutoScalingGroup  (..)

   -- ** UpdateAutoScalingGroup
   , UpdateAutoScalingGroup               (..)

   -- * Data Types
   , module Network.AWS.AutoScaling.Types
   , Rs                                   (..)
   ) where

import Data.ByteString               (ByteString)
import Data.Monoid
import Data.Text                     (Text)
import Data.Time
import Network.AWS.AutoScaling.Types
import Network.AWS.Internal
import Network.Http.Client           (Method(..))

qry :: IsQuery a => Method -> ByteString -> a -> RawRequest
qry meth act q = queryAppend (queryRequest autoScalingService meth "/" q)
    [ ("Action", act)
    ]

--
-- Actions
--

-- | Creates a new Auto Scaling group with the specified name and other
-- attributes. When the creation request is completed, the Auto Scaling group
-- is ready to be used in other calls. Note The Auto Scaling group name must
-- be unique within the scope of your AWS account.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateAutoScalingGroup.html>
data CreateAutoScalingGroup = CreateAutoScalingGroup
    { casgAutoScalingGroupName    :: !Text
      -- ^ The name of the Auto Scaling group.
    , casgAvailabilityZones       :: Members Text
      -- ^ A list of Availability Zones for the Auto Scaling group. This is
      -- required unless you have specified subnets.
    , casgDefaultCooldown         :: Maybe Integer
      -- ^ The amount of time, in seconds, between a successful scaling
      -- activity and the succeeding scaling activity.
    , casgDesiredCapacity         :: Maybe Integer
      -- ^ The number of Amazon EC2 instances that should be running in the
      -- group. The desired capacity must be greater than or equal to the
      -- minimum size and less than or equal to the maximum size specified
      -- for the Auto Scaling group.
    , casgHealthCheckGracePeriod  :: Maybe Integer
      -- ^ Length of time in seconds after a new Amazon EC2 instance comes
      -- into service that Auto Scaling starts checking its health. During
      -- this time any health check failure for the that instance is
      -- ignored.
    , casgHealthCheckType         :: Maybe Text
      -- ^ The service you want the health checks from, Amazon EC2 or
      -- Elastic Load Balancer. Valid values are EC2 or ELB.
    , casgLaunchConfigurationName :: !Text
      -- ^ The name of an existing launch configuration to use to launch new
      -- instances.
    , casgLoadBalancerNames       :: Members Text
      -- ^ A list of existing Elastic Load Balancing load balancers to use.
      -- The load balancers must be associated with the AWS account.
    , casgMaxSize                 :: !Integer
      -- ^ The maximum size of the Auto Scaling group.
    , casgMinSize                 :: !Integer
      -- ^ The minimum size of the Auto Scaling group.
    , casgPlacementGroup          :: Maybe Text
      -- ^ Physical location of an existing cluster placement group into
      -- which you want to launch your instances. For information about
      -- cluster placement group, see Using Cluster Instances
    , casgTags                    :: Members Tag
      -- ^ The tag to be created or updated. Each tag should be defined by
      -- its resource type, resource ID, key, value, and a propagate flag.
      -- Valid values: key=value, value=value, propagate=true or false.
      -- Value and propagate are optional parameters.
    , casgTerminationPolicies     :: Members Text
      -- ^ A standalone termination policy or a list of termination policies
      -- used to select the instance to terminate. The policies are
      -- executed in the order that they are listed.
    , casgVPCZoneIdentifier       :: Maybe Text
      -- ^ A comma-separated list of subnet identifiers of Amazon Virtual
      -- Private Clouds (Amazon VPCs).
    } deriving (Eq, Show, Generic)

instance IsQuery CreateAutoScalingGroup

instance Rq CreateAutoScalingGroup where
    request = qry GET "CreateAutoScalingGroup"

type instance Er CreateAutoScalingGroup = AutoScalingError
data instance Rs CreateAutoScalingGroup = CreateAutoScalingGroupResponse
    { casgrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs CreateAutoScalingGroup) where
    xmlPickler = withNS autoScalingNS

-- | Creates a new launch configuration. The launch configuration name must be
-- unique within the scope of the client's AWS account. The maximum limit of
-- launch configurations, which by default is 100, must not yet have been met;
-- otherwise, the call will fail. When created, the new launch configuration
-- is available for immediate use. You can create a launch configuration with
-- Amazon EC2 security groups or with Amazon VPC security groups. However, you
-- can't use Amazon EC2 security groups together with Amazon VPC security
-- groups, or vice versa. Note At this time, Auto Scaling launch
-- configurations don't support compressed (e.g. zipped) user data files.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateLaunchConfiguration.html>
data CreateLaunchConfiguration = CreateLaunchConfiguration
    { clcBlockDeviceMappings     :: Members BlockDeviceMapping
      -- ^ A list of mappings that specify how block devices are exposed to
      -- the instance. Each mapping is made up of a VirtualName, a
      -- DeviceName, and an ebs data structure that contains information
      -- about the associated Elastic Block Storage volume. For more
      -- information about Amazon EC2 BlockDeviceMappings, go to Block
      -- Device Mapping in the Amazon EC2 product documentation.
    , clcEbsOptimized            :: Maybe Bool
      -- ^ Whether the instance is optimized for EBS I/O. The optimization
      -- provides dedicated throughput to Amazon EBS and an optimized
      -- configuration stack to provide optimal EBS I/O performance. This
      -- optimization is not available with all instance types. Additional
      -- usage charges apply when using an EBS Optimized instance. By
      -- default the instance is not optimized for EBS I/O. For
      -- information about EBS-optimized instances, go to EBS-Optimized
      -- Instances in the Amazon Elastic Compute Cloud User Guide.
    , clcIamInstanceProfile      :: Maybe Text
      -- ^ The name or the Amazon Resource Name (ARN) of the instance
      -- profile associated with the IAM role for the instance.
    , clcImageId                 :: !Text
      -- ^ Unique ID of the Amazon Machine Image (AMI) you want to use to
      -- launch your EC2 instances. For information about finding Amazon
      -- EC2 AMIs, see Finding a Suitable AMI in the Amazon Elastic
      -- Compute Cloud User Guide.
    , clcInstanceMonitoring      :: Maybe InstanceMonitoring
      -- ^ Enables detailed monitoring if it is disabled. Detailed
      -- monitoring is enabled by default.
    , clcInstanceType            :: !Text
      -- ^ The instance type of the Amazon EC2 instance. For information
      -- about available Amazon EC2 instance types, see Available Instance
      -- Types in the Amazon Elastic Cloud Compute User Guide.
    , clcKernelId                :: Maybe Text
      -- ^ The ID of the kernel associated with the Amazon EC2 AMI.
    , clcKeyName                 :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair. For more information, see
      -- Getting a Key Pair in the Amazon Elastic Compute Cloud User
      -- Guide.
    , clcLaunchConfigurationName :: !Text
      -- ^ The name of the launch configuration to create.
    , clcRamdiskId               :: Maybe Text
      -- ^ The ID of the RAM disk associated with the Amazon EC2 AMI.
    , clcSecurityGroups          :: Members Text
      -- ^ The security groups with which to associate Amazon EC2 or Amazon
      -- VPC instances.
    , clcSpotPrice               :: Maybe Text
      -- ^ The maximum hourly price to be paid for any Spot Instance
      -- launched to fulfill the request. Spot Instances are launched when
      -- the price you specify exceeds the current Spot market price. For
      -- more information on launching Spot Instances, see Using Auto
      -- Scaling to Launch Spot Instances in the Auto Scaling Developer
      -- Guide.
    , clcUserData                :: Maybe Text
      -- ^ The user data to make available to the launched Amazon EC2
      -- instances. For more information about Amazon EC2 user data, see
      -- User Data Retrieval in the Amazon Elastic Compute Cloud User
      -- Guide.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateLaunchConfiguration

instance Rq CreateLaunchConfiguration where
    request = qry GET "CreateLaunchConfiguration"

type instance Er CreateLaunchConfiguration = AutoScalingError
data instance Rs CreateLaunchConfiguration = CreateLaunchConfigurationResponse
    { clcrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs CreateLaunchConfiguration) where
    xmlPickler = withNS autoScalingNS

-- | Creates new tags or updates existing tags for an Auto Scaling group. Note A
-- tag's definition is composed of a resource ID, resource type, key and
-- value, and the propagate flag. Value and the propagate flag are optional
-- parameters. See the Request Parameters for more information. For
-- information on creating tags for your Auto Scaling group, see Tag Your Auto
-- Scaling Groups and Amazon EC2 Instances.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateOrUpdateTags.html>
data CreateOrUpdateTags = CreateOrUpdateTags
    { coutTags :: Members Tag
      -- ^ The tag to be created or updated. Each tag should be defined by
      -- its resource type, resource ID, key, value, and a propagate flag.
      -- The resource type and resource ID identify the type and name of
      -- resource for which the tag is created. Currently,
      -- auto-scaling-group is the only supported resource type. The valid
      -- value for the resource ID is groupname.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateOrUpdateTags

instance Rq CreateOrUpdateTags where
    request = qry GET "CreateOrUpdateTags"

type instance Er CreateOrUpdateTags = AutoScalingError
data instance Rs CreateOrUpdateTags = CreateOrUpdateTagsResponse
    { coutrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs CreateOrUpdateTags) where
    xmlPickler = withNS autoScalingNS

-- | Deletes the specified Auto Scaling group if the group has no instances and
-- no scaling activities in progress. Note To remove all instances before
-- calling DeleteAutoScalingGroup, you can call UpdateAutoScalingGroup to set
-- the minimum and maximum size of the AutoScalingGroup to zero.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteAutoScalingGroup.html>
data DeleteAutoScalingGroup = DeleteAutoScalingGroup
    { dasgAutoScalingGroupName :: !Text
      -- ^ The name of the Auto Scaling group to delete.
    , dasgForceDelete          :: Maybe Bool
      -- ^ Starting with API version 2011-01-01, specifies that the Auto
      -- Scaling group will be deleted along with all instances associated
      -- with the group, without waiting for all instances to be
      -- terminated.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAutoScalingGroup

instance Rq DeleteAutoScalingGroup where
    request = qry GET "DeleteAutoScalingGroup"

type instance Er DeleteAutoScalingGroup = AutoScalingError
data instance Rs DeleteAutoScalingGroup = DeleteAutoScalingGroupResponse
    { dasgrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DeleteAutoScalingGroup) where
    xmlPickler = withNS autoScalingNS

-- | Deletes the specified LaunchConfiguration. The specified launch
-- configuration must not be attached to an Auto Scaling group. When this call
-- completes, the launch configuration is no longer available for use.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteLaunchConfiguration.html>
data DeleteLaunchConfiguration = DeleteLaunchConfiguration
    { dlcLaunchConfigurationName :: !Text
      -- ^ The name of the launch configuration.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteLaunchConfiguration

instance Rq DeleteLaunchConfiguration where
    request = qry GET "DeleteLaunchConfiguration"

type instance Er DeleteLaunchConfiguration = AutoScalingError
data instance Rs DeleteLaunchConfiguration = DeleteLaunchConfigurationResponse
    { dlcrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DeleteLaunchConfiguration) where
    xmlPickler = withNS autoScalingNS

-- | Deletes notifications created by PutNotificationConfiguration.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteNotificationConfiguration.html>
data DeleteNotificationConfiguration = DeleteNotificationConfiguration
    { dncAutoScalingGroupName :: !Text
      -- ^ The name of the Auto Scaling group.
    , dncTopicARN             :: !Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteNotificationConfiguration

instance Rq DeleteNotificationConfiguration where
    request = qry GET "DeleteNotificationConfiguration"

type instance Er DeleteNotificationConfiguration = AutoScalingError
data instance Rs DeleteNotificationConfiguration = DeleteNotificationConfigurationResponse
    deriving (Eq, Read, Show, Generic)

instance IsXML (Rs DeleteNotificationConfiguration) where
    xmlPickler = xpEmpty $ Just autoScalingNS

-- | Deletes a policy created by PutScalingPolicy.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeletePolicy.html>
data DeletePolicy = DeletePolicy
    { dpAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , dpPolicyName           :: !Text
      -- ^ The name or PolicyARN of the policy you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeletePolicy

instance Rq DeletePolicy where
    request = qry GET "DeletePolicy"

type instance Er DeletePolicy = AutoScalingError
data instance Rs DeletePolicy = DeletePolicyResponse
    deriving (Eq, Read, Show, Generic)

instance IsXML (Rs DeletePolicy) where
    xmlPickler = xpEmpty $ Just autoScalingNS

-- | Deletes a scheduled action previously created using the
-- PutScheduledUpdateGroupAction.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteScheduledAction.html>
data DeleteScheduledAction = DeleteScheduledAction
    { dsaAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , dsaScheduledActionName  :: !Text
      -- ^ The name of the action you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteScheduledAction

instance Rq DeleteScheduledAction where
    request = qry GET "DeleteScheduledAction"

type instance Er DeleteScheduledAction = AutoScalingError
data instance Rs DeleteScheduledAction = DeleteScheduledActionResponse
    deriving (Eq, Read, Show, Generic)

instance IsXML (Rs DeleteScheduledAction) where
    xmlPickler = xpEmpty $ Just autoScalingNS

-- | Removes the specified tags or a set of tags from a set of resources.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteTags.html>
data DeleteTags = DeleteTags
    { dtTags :: Members Tag
      -- ^ Each tag should be defined by its resource type, resource ID,
      -- key, value, and a propagate flag. Valid values are: Resource type
      -- = auto-scaling-group, Resource ID = AutoScalingGroupName,
      -- key=value, value=value, propagate=true or false.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteTags

instance Rq DeleteTags where
    request = qry GET "DeleteTags"

type instance Er DeleteTags = AutoScalingError
data instance Rs DeleteTags = DeleteTagsResponse
    deriving (Eq, Read, Show, Generic)

instance IsXML (Rs DeleteTags) where
    xmlPickler = xpEmpty $ Just autoScalingNS

-- | Returns policy adjustment types for use in the PutScalingPolicy action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAdjustmentTypes.html>
data DescribeAdjustmentTypes = DescribeAdjustmentTypes
    deriving (Eq, Show, Generic)

instance IsQuery DescribeAdjustmentTypes

instance Rq DescribeAdjustmentTypes where
    request = qry GET "DescribeAdjustmentTypes"

type instance Er DescribeAdjustmentTypes = AutoScalingError
data instance Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse
    { datrDescribeAdjustmentTypesResult :: !DescribeAdjustmentTypesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeAdjustmentTypes) where
    xmlPickler = withNS autoScalingNS

-- | Returns a full description of each Auto Scaling group in the given list.
-- This includes all Amazon EC2 instances that are members of the group. If a
-- list of names is not provided, the service returns the full details of all
-- Auto Scaling groups. This action supports pagination by returning a token
-- if there are more pages to retrieve. To get the next page, call this action
-- again with the returned token as the NextToken parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroups.html>
data DescribeAutoScalingGroups = DescribeAutoScalingGroups
    { dasgAutoScalingGroupNames :: Members Text
      -- ^ A list of Auto Scaling group names.
    , dasgMaxRecords            :: Maybe Integer
      -- ^ The maximum number of records to return.
    , dasgNextToken             :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAutoScalingGroups

instance Rq DescribeAutoScalingGroups where
    request = qry GET "DescribeAutoScalingGroups"

type instance Er DescribeAutoScalingGroups = AutoScalingError
data instance Rs DescribeAutoScalingGroups = DescribeAutoScalingGroupsResponse
    { dashrDescribeAutoScalingGroupsResult :: !DescribeAutoScalingGroupsResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeAutoScalingGroups) where
    xmlPickler = withNS autoScalingNS

-- | Returns a description of each Auto Scaling instance in the InstanceIds
-- list. If a list is not provided, the service returns the full details of
-- all instances up to a maximum of 50. By default, the service returns a list
-- of 20 items. This action supports pagination by returning a token if there
-- are more pages to retrieve. To get the next page, call this action again
-- with the returned token as the NextToken parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingInstances.html>
data DescribeAutoScalingInstances = DescribeAutoScalingInstances
    { dasiInstanceIds :: Members Text
      -- ^ The list of Auto Scaling instances to describe. If this list is
      -- omitted, all auto scaling instances are described. The list of
      -- requested instances cannot contain more than 50 items. If unknown
      -- instances are requested, they are ignored with no error.
    , dasiMaxRecords  :: Maybe Integer
      -- ^ The maximum number of Auto Scaling instances to be described with
      -- each call.
    , dasiNextToken   :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAutoScalingInstances

instance Rq DescribeAutoScalingInstances where
    request = qry GET "DescribeAutoScalingInstances"

type instance Er DescribeAutoScalingInstances = AutoScalingError
data instance Rs DescribeAutoScalingInstances = DescribeAutoScalingInstancesResponse
    { dasirDescribeAutoScalingInstancesResult :: !DescribeAutoScalingInstancesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeAutoScalingInstances) where
    xmlPickler = withNS autoScalingNS

-- | Returns a list of all notification types that are supported by Auto
-- Scaling.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingNotificationTypes.html>
data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    { dasntAutoScalingNotificationTypes :: !Text
      -- ^ Returns a list of all notification types supported by Auto
      -- Scaling. They are:
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAutoScalingNotificationTypes

instance Rq DescribeAutoScalingNotificationTypes where
    request = qry GET "DescribeAutoScalingNotificationTypes"

type instance Er DescribeAutoScalingNotificationTypes = AutoScalingError
data instance Rs DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypesResponse
    { dasntrDescribeAutoScalingNotificationTypesResult :: !DescribeAutoScalingNotificationTypesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeAutoScalingNotificationTypes) where
    xmlPickler = withNS autoScalingNS

-- | Returns a full description of the launch configurations, or the specified
-- launch configurations, if they exist. If no name is specified, then the
-- full details of all launch configurations are returned.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLaunchConfigurations.html>
data DescribeLaunchConfigurations = DescribeLaunchConfigurations
    { dlcLaunchConfigurationNames :: Members Text
      -- ^ A list of launch configuration names.
    , dlcMaxRecords               :: Maybe Integer
      -- ^ The maximum number of launch configurations. The default is 100.
    , dlcNextToken                :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeLaunchConfigurations

instance Rq DescribeLaunchConfigurations where
    request = qry GET "DescribeLaunchConfigurations"

type instance Er DescribeLaunchConfigurations = AutoScalingError
data instance Rs DescribeLaunchConfigurations = DescribeLaunchConfigurationsResponse
    { dldrDescribeLaunchConfigurationsResult :: !DescribeLaunchConfigurationsResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeLaunchConfigurations) where
    xmlPickler = withNS autoScalingNS

-- | Returns a list of metrics and a corresponding list of granularities for
-- each metric.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeMetricCollectionTypes.html>
data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Eq, Show, Generic)

instance IsQuery DescribeMetricCollectionTypes

instance Rq DescribeMetricCollectionTypes where
    request = qry GET "DescribeMetricCollectionTypes"

type instance Er DescribeMetricCollectionTypes = AutoScalingError
data instance Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesResponse
    { dmctDescribeMetricCollectionTypesResult :: !DescribeMetricCollectionTypesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeMetricCollectionTypes) where
    xmlPickler = withNS autoScalingNS

-- | Returns a list of notification actions associated with Auto Scaling groups
-- for specified events.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeNotificationConfigurations.html>
data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { dncAutoScalingGroupNames :: Members Text
      -- ^ The name of the Auto Scaling group.
    , dncMaxRecords            :: Maybe Integer
      -- ^ Maximum number of records to be returned.
    , dncNextToken             :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeNotificationConfigurations

instance Rq DescribeNotificationConfigurations where
    request = qry GET "DescribeNotificationConfigurations"

type instance Er DescribeNotificationConfigurations = AutoScalingError
data instance Rs DescribeNotificationConfigurations = DescribeNotificationConfigurationsResponse
    { dndrDescribeNotificationConfigurationsResult :: !DescribeNotificationConfigurationsResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeNotificationConfigurations) where
    xmlPickler = withNS autoScalingNS

-- | Returns descriptions of what each policy does. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribePolicies.html>
data DescribePolicies = DescribePolicies
    { dqAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , dqMaxRecords           :: Maybe Integer
      -- ^ The maximum number of policies that will be described with each
      -- call.
    , dqNextToken            :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    , dqPolicyNames          :: Members Text
      -- ^ A list of policy names or policy ARNs to be described. If this
      -- list is omitted, all policy names are described. If an auto
      -- scaling group name is provided, the results are limited to that
      -- group. The list of requested policy names cannot contain more
      -- than 50 items. If unknown policy names are requested, they are
      -- ignored with no error.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribePolicies

instance Rq DescribePolicies where
    request = qry GET "DescribePolicies"

type instance Er DescribePolicies = AutoScalingError
data instance Rs DescribePolicies = DescribePoliciesResponse
    { dqrDescribePoliciesResult :: !DescribePoliciesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribePolicies) where
    xmlPickler = withNS autoScalingNS

-- | Returns the scaling activities for the specified Auto Scaling group. If the
-- specified ActivityIds list is empty, all the activities from the past six
-- weeks are returned. Activities are sorted by completion time. Activities
-- still in progress appear first on the list. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingActivities.html>
data DescribeScalingActivities = DescribeScalingActivities
    { dsbActivityIds          :: Members Text
      -- ^ A list containing the activity IDs of the desired scaling
      -- activities. If this list is omitted, all activities are
      -- described. If an AutoScalingGroupName is provided, the results
      -- are limited to that group. The list of requested activities
      -- cannot contain more than 50 items. If unknown activities are
      -- requested, they are ignored with no error.
    , dsbAutoScalingGroupName :: Maybe Text
      -- ^ The name of the AutoScalingGroup.
    , dsbMaxRecords           :: Maybe Integer
      -- ^ The maximum number of scaling activities to return.
    , dsbNextToken            :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results for pagination.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeScalingActivities

instance Rq DescribeScalingActivities where
    request = qry GET "DescribeScalingActivities"

type instance Er DescribeScalingActivities = AutoScalingError
data instance Rs DescribeScalingActivities = DescribeScalingActivitiesResponse
    { dsbrDescribeScalingActivitiesResult :: !DescribeScalingActivitiesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeScalingActivities) where
    xmlPickler = withNS autoScalingNS

-- | Returns scaling process types for use in the ResumeProcesses and
-- SuspendProcesses actions.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingProcessTypes.html>
data DescribeScalingProcessTypes = DescribeScalingProcessTypes
    { dsptProcesses :: !ProcessType
      -- ^ A list of ProcessType names.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeScalingProcessTypes

instance Rq DescribeScalingProcessTypes where
    request = qry GET "DescribeScalingProcessTypes"

type instance Er DescribeScalingProcessTypes = AutoScalingError
data instance Rs DescribeScalingProcessTypes = DescribeScalingProcessTypesResponse
    { dsptrDescribeScalingProcessTypesResult :: !DescribeScalingProcessTypesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeScalingProcessTypes) where
    xmlPickler = withNS autoScalingNS

-- | Lists all the actions scheduled for your Auto Scaling group that haven't
-- been executed. To see a list of actions already executed, see the activity
-- record returned in DescribeScalingActivities.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScheduledActions.html>
data DescribeScheduledActions = DescribeScheduledActions
    { dscAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , dscEndTime              :: Maybe UTCTime
      -- ^ The latest scheduled start time to return. If scheduled action
      -- names are provided, this field is ignored.
    , dscMaxRecords           :: Maybe Integer
      -- ^ The maximum number of scheduled actions to return.
    , dscNextToken            :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    , dscScheduledActionNames :: Members Text
      -- ^ A list of scheduled actions to be described. If this list is
      -- omitted, all scheduled actions are described. The list of
      -- requested scheduled actions cannot contain more than 50 items. If
      -- an auto scaling group name is provided, the results are limited
      -- to that group. If unknown scheduled actions are requested, they
      -- are ignored with no error.
    , dscStartTime            :: Maybe UTCTime
      -- ^ The earliest scheduled start time to return. If scheduled action
      -- names are provided, this field will be ignored.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeScheduledActions

instance Rq DescribeScheduledActions where
    request = qry GET "DescribeScheduledActions"

type instance Er DescribeScheduledActions = AutoScalingError
data instance Rs DescribeScheduledActions = DescribeScheduledActionsResponse
    { dscrDescribeScheduledActionsResult :: !DescribeScheduledActionsResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeScheduledActions) where
    xmlPickler = withNS autoScalingNS

-- | Lists the Auto Scaling group tags. You can use filters to limit results
-- when describing tags. For example, you can query for tags of a particular
-- Auto Scaling group. You can specify multiple values for a filter. A tag
-- must match at least one of the specified values for it to be included in
-- the results. You can also specify multiple filters. The result includes
-- information for a particular tag only if it matches all your filters. If
-- there's no match, no special message is returned.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTags.html>
data DescribeTags = DescribeTags
    { dtFilters    :: Members Filter
      -- ^ The value of the filter type used to identify the tags to be
      -- returned. For example, you can filter so that tags are returned
      -- according to Auto Scaling group, the key and value, or whether
      -- the new tag will be applied to instances launched after the tag
      -- is created (PropagateAtLaunch).
    , dtMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to return.
    , dtNextToken  :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeTags

instance Rq DescribeTags where
    request = qry GET "DescribeTags"

type instance Er DescribeTags = AutoScalingError
data instance Rs DescribeTags = DescribeTagsResponse
    { dtrDescribeTagsResult :: !DescribeTagsResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeTags) where
    xmlPickler = withNS autoScalingNS

-- | Returns a list of all termination policies supported by Auto Scaling.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTerminationPolicyTypes.html>
data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes
    { dtptTerminationPolicyTypes :: !Text
      -- ^ Termination policies supported by Auto Scaling. They are:
      -- OldestInstance, OldestLaunchConfiguration, NewestInstance,
      -- ClosestToNextInstanceHour, Default
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeTerminationPolicyTypes

instance Rq DescribeTerminationPolicyTypes where
    request = qry GET "DescribeTerminationPolicyTypes"

type instance Er DescribeTerminationPolicyTypes = AutoScalingError
data instance Rs DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypesResponse
    { dtptrDescribeTerminationPolicyTypesResult :: !DescribeTerminationPolicyTypesResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DescribeTerminationPolicyTypes) where
    xmlPickler = withNS autoScalingNS

-- | Disables monitoring of group metrics for the Auto Scaling group specified
-- in AutoScalingGroupName. You can specify the list of affected metrics with
-- the Metrics parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DisableMetricsCollection.html>
data DisableMetricsCollection = DisableMetricsCollection
    { dmcAutoScalingGroupName :: !Text
      -- ^ The name or ARN of the Auto Scaling Group.
    , dmcMetrics              :: Members Text
      -- ^ The list of metrics to disable. If no metrics are specified, all
      -- metrics are disabled. The following metrics are supported:
    } deriving (Eq, Show, Generic)

instance IsQuery DisableMetricsCollection

instance Rq DisableMetricsCollection where
    request = qry GET "DisableMetricsCollection"

type instance Er DisableMetricsCollection = AutoScalingError
data instance Rs DisableMetricsCollection = DisableMetricsCollectionResponse
    { dmcrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs DisableMetricsCollection) where
    xmlPickler = withNS autoScalingNS

-- | Enables monitoring of group metrics for the Auto Scaling group specified in
-- AutoScalingGroupName. You can specify the list of enabled metrics with the
-- Metrics parameter. Auto Scaling metrics collection can be turned on only if
-- the InstanceMonitoring flag, in the Auto Scaling group's launch
-- configuration, is set to True.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnableMetricsCollection.html>
data EnableMetricsCollection = EnableMetricsCollection
    { emcAutoScalingGroupName :: !Text
      -- ^ The name or ARN of the Auto Scaling group.
    , emcGranularity          :: !Text
      -- ^ The granularity to associate with the metrics to collect.
      -- Currently, the only legal granularity is "1Minute".
    , emcMetrics              :: Members Text
      -- ^ The list of metrics to collect. If no metrics are specified, all
      -- metrics are enabled. The following metrics are supported:
    } deriving (Eq, Show, Generic)

instance IsQuery EnableMetricsCollection

instance Rq EnableMetricsCollection where
    request = qry GET "EnableMetricsCollection"

type instance Er EnableMetricsCollection = AutoScalingError
data instance Rs EnableMetricsCollection = EnableMetricsCollectionResponse
    { emcrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs EnableMetricsCollection) where
    xmlPickler = withNS autoScalingNS

-- | Executes the specified policy.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ExecutePolicy.html>
data ExecutePolicy = ExecutePolicy
    { epAutoScalingGroupName :: Maybe Text
      -- ^ The name or the Amazon Resource Name (ARN) of the Auto Scaling
      -- group.
    , epHonorCooldown        :: Maybe Bool
      -- ^ Set to True if you want Auto Scaling to wait for the cooldown
      -- period associated with the Auto Scaling group to complete before
      -- executing the policy.
    , epPolicyName           :: !Text
      -- ^ The name or ARN of the policy you want to run.
    } deriving (Eq, Show, Generic)

instance IsQuery ExecutePolicy

instance Rq ExecutePolicy where
    request = qry GET "ExecutePolicy"

type instance Er ExecutePolicy = AutoScalingError
data instance Rs ExecutePolicy = ExecutePolicyResponse
    { eprResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs ExecutePolicy) where
    xmlPickler = withNS autoScalingNS

-- | Configures an Auto Scaling group to send notifications when specified
-- events take place. Subscribers to this topic can have messages for events
-- delivered to an endpoint such as a web server or email address. For more
-- information see Get Email Notifications When Your Auto Scaling Group
-- Changes A new PutNotificationConfiguration overwrites an existing
-- configuration.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutNotificationConfiguration.html>
data PutNotificationConfiguration = PutNotificationConfiguration
    { pncAutoScalingGroupName :: !Text
      -- ^ The name of the Auto Scaling group.
    , pncNotificationTypes    :: Members Text
      -- ^ The type of event that will cause the notification to be sent.
      -- For details about notification types supported by Auto Scaling,
      -- see DescribeAutoScalingNotificationTypes.
    , pncTopicARN             :: !Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    } deriving (Eq, Show, Generic)

instance IsQuery PutNotificationConfiguration

instance Rq PutNotificationConfiguration where
    request = qry GET "PutNotificationConfiguration"

type instance Er PutNotificationConfiguration = AutoScalingError
data instance Rs PutNotificationConfiguration = PutNotificationConfigurationResponse
    { pncrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs PutNotificationConfiguration) where
    xmlPickler = withNS autoScalingNS

-- | Creates or updates a policy for an Auto Scaling group. To update an
-- existing policy, use the existing policy name and set the parameter(s) you
-- want to change. Any existing parameter not changed in an update to an
-- existing policy is not changed in this update request.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScalingPolicy.html>
data PutScalingPolicy = PutScalingPolicy
    { pspAdjustmentType       :: !Text
      -- ^ Specifies whether the ScalingAdjustment is an absolute number or
      -- a percentage of the current capacity. Valid values are
      -- ChangeInCapacity, ExactCapacity, and PercentChangeInCapacity.
    , pspAutoScalingGroupName :: !Text
      -- ^ The name or ARN of the Auto Scaling group.
    , pspCooldown             :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes and before the next scaling acitvity can start.
    , pspMinAdjustmentStep    :: Maybe Integer
      -- ^ Used with AdjustmentType with the value PercentChangeInCapacity,
      -- the scaling policy changes the DesiredCapacity of the Auto
      -- Scaling group by at least the number of instances specified in
      -- the value.
    , pspPolicyName           :: !Text
      -- ^ The name of the policy you want to create or update.
    , pspScalingAdjustment    :: !Integer
      -- ^ The number of instances by which to scale. AdjustmentType
      -- determines the interpretation of this number (e.g., as an
      -- absolute number or as a percentage of the existing Auto Scaling
      -- group size). A positive increment adds to the current capacity
      -- and a negative value removes from the current capacity.
    } deriving (Eq, Show, Generic)

instance IsQuery PutScalingPolicy

instance Rq PutScalingPolicy where
    request = qry GET "PutScalingPolicy"

type instance Er PutScalingPolicy = AutoScalingError
data instance Rs PutScalingPolicy = PutScalingPolicyResponse
    { psprPutScalingPolicyResult :: !PutScalingPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs PutScalingPolicy) where
    xmlPickler = withNS autoScalingNS

-- | Creates or updates a scheduled scaling action for an Auto Scaling group.
-- When updating a scheduled scaling action, if you leave a parameter
-- unspecified, the corresponding value remains unchanged in the affected Auto
-- Scaling group. For information on creating or updating a scheduled action
-- for your Auto Scaling group, see Scale Based on a Schedule. Note Auto
-- Scaling supports the date and time expressed in "YYYY-MM-DDThh:mm:ssZ"
-- format in UTC/GMT only.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScheduledUpdateGroupAction.html>
data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction
    { psugaAutoScalingGroupName :: !Text
      -- ^ The name or ARN of the Auto Scaling group.
    , psugaDesiredCapacity      :: Maybe Integer
      -- ^ The number of Amazon EC2 instances that should be running in the
      -- group.
    , psugaEndTime              :: Maybe UTCTime
      -- ^ The time for this action to end.
    , psugaMaxSize              :: Maybe Integer
      -- ^ The maximum size for the Auto Scaling group.
    , psugaMinSize              :: Maybe Integer
      -- ^ The minimum size for the new Auto Scaling group.
    , psugaRecurrence           :: Maybe Text
      -- ^ The time when recurring future actions will start. Start time is
      -- specified by the user following the Unix cron syntax format. For
      -- information about cron syntax, go to Wikipedia, The Free
      -- Encyclopedia.
    , psugaScheduledActionName  :: !Text
      -- ^ The name of this scaling action.
    , psugaStartTime            :: Maybe UTCTime
      -- ^ The time for this action to start, as in --start-time
      -- 2010-06-01T00:00:00Z.
    , psugaTime                 :: Maybe UTCTime
      -- ^ Time is deprecated.
    } deriving (Eq, Show, Generic)

instance IsQuery PutScheduledUpdateGroupAction

instance Rq PutScheduledUpdateGroupAction where
    request = qry GET "PutScheduledUpdateGroupAction"

type instance Er PutScheduledUpdateGroupAction = AutoScalingError
data instance Rs PutScheduledUpdateGroupAction = PutScheduledUpdateGroupActionResponse
    { psugarResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs PutScheduledUpdateGroupAction) where
    xmlPickler = withNS autoScalingNS

-- | Resumes all suspended Auto Scaling processes for an Auto Scaling group. For
-- information on suspending and resuming Auto Scaling process, see Suspend
-- and Resume Auto Scaling Process.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ResumeProcesses.html>
data ResumeProcesses = ResumeProcesses
    { rpAutoScalingGroupName :: !Text
      -- ^ The name or Amazon Resource Name (ARN) of the Auto Scaling group.
    , rpScalingProcesses     :: Members Text
      -- ^ The processes that you want to suspend or resume, which can
      -- include one or more of the following:
    } deriving (Eq, Show, Generic)

instance IsQuery ResumeProcesses

instance Rq ResumeProcesses where
    request = qry GET "ResumeProcesses"

type instance Er ResumeProcesses = AutoScalingError
data instance Rs ResumeProcesses = ResumeProcessesResponse
    { rprResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs ResumeProcesses) where
    xmlPickler = withNS autoScalingNS

-- | Sets the desired size of the specified AutoScalingGroup.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetDesiredCapacity.html>
data SetDesiredCapacity = SetDesiredCapacity
    { sdcAutoScalingGroupName :: !Text
      -- ^ The name of the Auto Scaling group.
    , sdcDesiredCapacity      :: !Integer
      -- ^ The new capacity setting for the Auto Scaling group.
    , sdcHonorCooldown        :: Maybe Bool
      -- ^ By default, SetDesiredCapacity overrides any cooldown period
      -- associated with the Auto Scaling group. Set to True if you want
      -- Auto Scaling to wait for the cooldown period associated with the
      -- Auto Scaling group to complete before initiating a scaling
      -- activity to set your Auto Scaling group to the new capacity
      -- setting.
    } deriving (Eq, Show, Generic)

instance IsQuery SetDesiredCapacity

instance Rq SetDesiredCapacity where
    request = qry GET "SetDesiredCapacity"

type instance Er SetDesiredCapacity = AutoScalingError
data instance Rs SetDesiredCapacity = SetDesiredCapacityResponse
    { sdcrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs SetDesiredCapacity) where
    xmlPickler = withNS autoScalingNS

-- | Sets the health status of a specified instance that belongs to any of your
-- Auto Scaling groups. For more information, see Configure Health Checks for
-- Your Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetInstanceHealth.html>
data SetInstanceHealth = SetInstanceHealth
    { sihHealthStatus             :: !Text
      -- ^ The health status of the instance. Set to Healthy if you want the
      -- instance to remain in service. Set to Unhealthy if you want the
      -- instance to be out of service. Auto Scaling will terminate and
      -- replace the unhealthy instance.
    , sihInstanceId               :: !Text
      -- ^ The identifier of the Amazon EC2 instance.
    , sihShouldRespectGracePeriod :: Maybe Bool
      -- ^ If the Auto Scaling group of the specified instance has a
      -- HealthCheckGracePeriod specified for the group, by default, this
      -- call will respect the grace period. Set this to False, if you do
      -- not want the call to respect the grace period associated with the
      -- group.
    } deriving (Eq, Show, Generic)

instance IsQuery SetInstanceHealth

instance Rq SetInstanceHealth where
    request = qry GET "SetInstanceHealth"

type instance Er SetInstanceHealth = AutoScalingError
data instance Rs SetInstanceHealth = SetInstanceHealthResponse
    { sihrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs SetInstanceHealth) where
    xmlPickler = withNS autoScalingNS

-- | Suspends Auto Scaling processes for an Auto Scaling group. To suspend
-- specific process types, specify them by name with the
-- ScalingProcesses.member.N parameter. To suspend all process types, omit the
-- ScalingProcesses.member.N parameter. Important Suspending either of the two
-- primary process types, Launch or Terminate, can prevent other process types
-- from functioning properly. To resume processes that have been suspended,
-- use ResumeProcesses For more information on suspending and resuming Auto
-- Scaling process, see Suspend and Resume Auto Scaling Process.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SuspendProcesses.html>
data SuspendProcesses = SuspendProcesses
    { spAutoScalingGroupName :: !Text
      -- ^ The name or Amazon Resource Name (ARN) of the Auto Scaling group.
    , spScalingProcesses     :: Members Text
      -- ^ The processes that you want to suspend or resume, which can
      -- include one or more of the following:
    } deriving (Eq, Show, Generic)

instance IsQuery SuspendProcesses

instance Rq SuspendProcesses where
    request = qry GET "SuspendProcesses"

type instance Er SuspendProcesses = AutoScalingError
data instance Rs SuspendProcesses = SuspendProcessesResponse
    { sprResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs SuspendProcesses) where
    xmlPickler = withNS autoScalingNS

-- | Terminates the specified instance. Optionally, the desired group size can
-- be adjusted. Note This call simply registers a termination request. The
-- termination of the instance cannot happen immediately.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_TerminateInstanceInAutoScalingGroup.html>
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
    { tiiasgInstanceId                     :: !Text
      -- ^ The ID of the Amazon EC2 instance to be terminated.
    , tiiasgShouldDecrementDesiredCapacity :: !Bool
      -- ^ Specifies whether (true) or not (false) terminating this instance
      -- should also decrement the size of the AutoScalingGroup.
    } deriving (Eq, Show, Generic)

instance IsQuery TerminateInstanceInAutoScalingGroup

instance Rq TerminateInstanceInAutoScalingGroup where
    request = qry GET "TerminateInstanceInAutoScalingGroup"

type instance Er TerminateInstanceInAutoScalingGroup = AutoScalingError
data instance Rs TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroupResponse
    { tiiasgrResponseMetadata                          :: !ResponseMetadata
    , tiiasgrTerminateInstanceInAutoScalingGroupResult :: !TerminateInstanceInAutoScalingGroupResult
    } deriving (Eq, Show, Generic)

instance IsXML (Rs TerminateInstanceInAutoScalingGroup) where
    xmlPickler = withNS autoScalingNS

-- | Updates the configuration for the specified AutoScalingGroup.
--
-- Note: To update an Auto Scaling group with a launch configuration that has the
-- InstanceMonitoring flag set to False, you must first ensure that collection
-- of group metrics is disabled. Otherwise, calls to UpdateAutoScalingGroup
-- will fail. If you have previously enabled group metrics collection, you can
-- disable collection of all group metrics by calling
-- DisableMetricsCollection. The new settings are registered upon the
-- completion of this call. Any launch configuration settings take effect on
-- any triggers after this call returns.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_UpdateAutoScalingGroup.html>
data UpdateAutoScalingGroup = UpdateAutoScalingGroup
    { uasgAutoScalingGroupName    :: !Text
      -- ^ The name of the Auto Scaling group.
    , uasgAvailabilityZones       :: Members Text
      -- ^ Availability Zones for the group.
    , uasgDefaultCooldown         :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes before any further scaling activities can start. For
      -- more information, see Cooldown Period.
    , uasgDesiredCapacity         :: Maybe Integer
      -- ^ The desired capacity for the Auto Scaling group.
    , uasgHealthCheckGracePeriod  :: Maybe Integer
      -- ^ The length of time that Auto Scaling waits before checking an
      -- instance's health status. The grace period begins when an
      -- instance comes into service.
    , uasgHealthCheckType         :: Maybe Text
      -- ^ The type of health check for the instances in the Auto Scaling
      -- group. The health check type can either be EC2 for Amazon EC2 or
      -- ELB for Elastic Load Balancing.
    , uasgLaunchConfigurationName :: Maybe Text
      -- ^ The name of the launch configuration.
    , uasgMaxSize                 :: Maybe Integer
      -- ^ The maximum size of the Auto Scaling group.
    , uasgMinSize                 :: Maybe Integer
      -- ^ The minimum size of the Auto Scaling group.
    , uasgPlacementGroup          :: Maybe Text
      -- ^ The name of the cluster placement group, if applicable. For more
      -- information, go to Using Cluster Instances in the Amazon EC2 User
      -- Guide.
    , uasgTerminationPolicies     :: Members Text
      -- ^ A standalone termination policy or a list of termination policies
      -- used to select the instance to terminate. The policies are
      -- executed in the order that they are listed.
    , uasgVPCZoneIdentifier       :: Maybe Text
      -- ^ The subnet identifier for the Amazon VPC connection, if
      -- applicable. You can specify several subnets in a comma-separated
      -- list.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateAutoScalingGroup

instance Rq UpdateAutoScalingGroup where
    request = qry GET "UpdateAutoScalingGroup"

type instance Er UpdateAutoScalingGroup = AutoScalingError
data instance Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse
    { uasgrResponseMetadata :: !ResponseMetadata
    } deriving (Eq, Show, Generic)

instance IsXML (Rs UpdateAutoScalingGroup) where
    xmlPickler = withNS autoScalingNS
