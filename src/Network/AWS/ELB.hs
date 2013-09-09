{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.ELB
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Elastic Load Balancing helps you improve the availability and scalability
-- of your application running on Amazon Elastic Cloud Compute (Amazon EC2).
module Network.AWS.ELB
   (
   -- * Actions
   -- ** ApplySecurityGroupsToLoadBalancer
     ApplySecurityGroupsToLoadBalancer       (..)

   -- ** AttachLoadBalancerToSubnets
   , AttachLoadBalancerToSubnets             (..)

   -- ** ConfigureHealthCheck
   , ConfigureHealthCheck                    (..)

   -- ** CreateAppCookieStickinessPolicy
   , CreateAppCookieStickinessPolicy         (..)

   -- ** CreatelbCookieStickinessPolicy
   , CreatelbCookieStickinessPolicy          (..)

   -- ** CreateLoadBalancer
   , CreateLoadBalancer                      (..)

   -- ** CreateLoadBalancerListeners
   , CreateLoadBalancerListeners             (..)

   -- ** CreateLoadBalancerPolicy
   , CreateLoadBalancerPolicy                (..)

   -- ** DeleteLoadBalancer
   , DeleteLoadBalancer                      (..)

   -- ** DeleteLoadBalancerListeners
   , DeleteLoadBalancerListeners             (..)

   -- ** DeleteLoadBalancerPolicy
   , DeleteLoadBalancerPolicy                (..)

   -- ** DeregisterInstancesFromLoadBalancer
   , DeregisterInstancesFromLoadBalancer     (..)

   -- ** DescribeInstanceHealth
   , DescribeInstanceHealth                  (..)

   -- ** DescribeLoadBalancerPolicies
   , DescribeLoadBalancerPolicies            (..)

   -- ** DescribeLoadBalancerPolicyTypes
   , DescribeLoadBalancerPolicyTypes         (..)

   -- ** DescribeLoadBalancers
   , DescribeLoadBalancers                   (..)

   -- ** DetachLoadBalancerFromSubnets
   , DetachLoadBalancerFromSubnets           (..)

   -- ** DisableAvailabilityZonesForLoadBalancer
   , DisableAvailabilityZonesForLoadBalancer (..)

   -- ** EnableAvailabilityZonesForLoadBalancer
   , EnableAvailabilityZonesForLoadBalancer  (..)

   -- ** RegisterInstancesWithLoadBalancer
   , RegisterInstancesWithLoadBalancer       (..)

   -- ** SetLoadBalancerListenerSSLCertificate
   , SetLoadBalancerListenerSSLCertificate   (..)

   -- ** SetLoadBalancerPoliciesForBackendServer
   , SetLoadBalancerPoliciesForBackendServer (..)

   -- ** SetLoadBalancerPoliciesOfListener
   , SetLoadBalancerPoliciesOfListener       (..)

   -- * Data Types
   , module Network.AWS.ELB.Types
   , Rs                                      (..)
   ) where

import Data.ByteString       (ByteString)
import Data.Monoid
import Data.Time
import Network.AWS.ELB.Types
import Network.AWS.Internal
import Network.Http.Client   (Method(..))

data ELB

instance AWSService ELB where
    service _ = awsService "elb" elbVersion SigningVersion4

req :: IsQuery a => Method -> ByteString -> a -> RawRequest ELB b
req meth act qry = (emptyRequest meth FormEncoded "/" Nothing)
    { rqAction = Just act
    , rqQuery  = toQuery qry
    }

--
-- Actions
--

-- | Associates one or more security groups with your load balancer in VPC. The
-- provided security group IDs will override any currently applied security
-- groups.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ApplySecurityGroupsToLoadBalancer.html>
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer
    { asgtlbLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    , asgtlbSecurityGroups   :: Members ByteString
      -- ^ A list of security group IDs to associate with your load balancer
      -- in VPC. The security group IDs must be provided as the ID and not
      -- the security group name (For example, sg-1234).
    } deriving (Eq, Show, Generic)

instance IsXML ApplySecurityGroupsToLoadBalancer where
    xmlPickler = withNS elbNS

instance IsQuery ApplySecurityGroupsToLoadBalancer

instance AWSRequest ELB ApplySecurityGroupsToLoadBalancer ApplySecurityGroupsToLoadBalancerResponse where
    request = req GET "ApplySecurityGroupsToLoadBalancer"

data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse
    { asgtlbrResponseMetadata :: !ByteString
    , asgtlbrApplySecurityGroupsToLoadBalancerResult :: !ApplySecurityGroupsToLoadBalancerResult
    } deriving (Eq, Show, Generic)

instance IsXML ApplySecurityGroupsToLoadBalancerResponse where
    xmlPickler = withNS elbNS

-- | Adds one or more subnets to the set of configured subnets in the VPC for
-- the load balancer. The Loadbalancers evenly distribute requests across all
-- of the registered subnets.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_AttachLoadBalancerToSubnets.html>
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets
    { albtsLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    , albtsSubnets          :: Members ByteString
      -- ^ A list of subnet IDs to add for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML AttachLoadBalancerToSubnets where
    xmlPickler = withNS elbNS

instance IsQuery AttachLoadBalancerToSubnets

instance AWSRequest ELB AttachLoadBalancerToSubnets AttachLoadBalancerToSubnetsResponse where
    request = req GET "AttachLoadBalancerToSubnets"

data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse
    { albtsrResponseMetadata :: !ByteString
    , albtsrAttachLoadBalancerToSubnetsResult :: !AttachLoadBalancerToSubnetsResult
    } deriving (Eq, Show, Generic)

instance IsXML AttachLoadBalancerToSubnetsResponse where
    xmlPickler = withNS elbNS

-- | Enables the client to define an application healthcheck for the instances.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ConfigureHealthCheck.html>
data ConfigureHealthCheck = ConfigureHealthCheck
    { chcHealthCheck      :: !HealthCheck
      -- ^ A structure containing the configuration information for the new
      -- healthcheck.
    , chcLoadBalancerName :: !ByteString
      -- ^ The mnemonic name associated with the load balancer. This name
      -- must be unique within the client AWS account.
    } deriving (Eq, Show, Generic)

instance IsXML ConfigureHealthCheck where
    xmlPickler = withNS elbNS

instance IsQuery ConfigureHealthCheck

instance AWSRequest ELB ConfigureHealthCheck ConfigureHealthCheckResponse where
    request = req GET "ConfigureHealthCheck"

data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { chcrResponseMetadata :: !ByteString
    , chcrConfigureHealthCheckResult :: !ConfigureHealthCheckResult
    } deriving (Eq, Show, Generic)

instance IsXML ConfigureHealthCheckResponse where
    xmlPickler = withNS elbNS

-- | Generates a stickiness policy with sticky session lifetimes that follow
-- that of an application-generated cookie. This policy can be associated only
-- with HTTP/HTTPS listeners. This policy is similar to the policy created by
-- CreatelbCookieStickinessPolicy, except that the lifetime of the special
-- Elastic Load Balancing cookie follows the lifetime of the
-- application-generated cookie specified in the policy configuration. The
-- load balancer only inserts a new stickiness cookie when the application
-- response includes a new application cookie. If the application cookie is
-- explicitly removed or expires, the session stops being sticky until a new
-- application cookie is issued. Note An application client must receive and
-- send two cookies: the application-generated cookie and the special Elastic
-- Load Balancing cookie named AWSELB. This is the default behavior for many
-- common web browsers. For information on using
-- CreateAppCookieStickinessPolicy, see Using the Query API in the Enabling
-- Application-Controlled Sesssion Stickiness section of the Elastic Load
-- Balancing Developer Guide.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateAppCookieStickinessPolicy.html>
data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy
    { cacspCookieName       :: !ByteString
      -- ^ Name of the application cookie used for stickiness.
    , cacspLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    , cacspPolicyName       :: !ByteString
      -- ^ The name of the policy being created. The name must be unique
      -- within the set of policies for this load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML CreateAppCookieStickinessPolicy where
    xmlPickler = withNS elbNS

instance IsQuery CreateAppCookieStickinessPolicy

instance AWSRequest ELB CreateAppCookieStickinessPolicy CreateAppCookieStickinessPolicyResponse where
    request = req GET "CreateAppCookieStickinessPolicy"

data CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse
    { cacsprResponseMetadata :: !ByteString
    , cacsprCreateAppCookieStickinessPolicyResult :: !CreateAppCookieStickinessPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateAppCookieStickinessPolicyResponse where
    xmlPickler = withNS elbNS

-- | Generates a stickiness policy with sticky session lifetimes controlled by
-- the lifetime of the browser (user-agent) or a specified expiration period.
-- This policy can be associated only with HTTP/HTTPS listeners. When a load
-- balancer implements this policy, the load balancer uses a special cookie to
-- track the backend server instance for each request. When the load balancer
-- receives a request, it first checks to see if this cookie is present in the
-- request. If so, the load balancer sends the request to the application
-- server specified in the cookie. If not, the load balancer sends the request
-- to a server that is chosen based on the existing load balancing algorithm.
-- A cookie is inserted into the response for binding subsequent requests from
-- the same user to that server. The validity of the cookie is based on the
-- cookie expiration time, which is specified in the policy configuration. For
-- information on using CreatelbCookieStickinessPolicy, see Using the Query
-- API in the Enabling Duration-Based Sesssion Stickiness section of the
-- Elastic Load Balancing Developer Guide.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreatelbCookieStickinessPolicy.html>
data CreatelbCookieStickinessPolicy = CreatelbCookieStickinessPolicy
    { clbcspCookieExpirationPeriod :: Maybe Integer
      -- ^ The time period in seconds after which the cookie should be
      -- considered stale. Not specifying this parameter indicates that
      -- the sticky session will last for the duration of the browser
      -- session.
    , clbcspLoadBalancerName       :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    , clbcspPolicyName             :: !ByteString
      -- ^ The name of the policy being created. The name must be unique
      -- within the set of policies for this load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML CreatelbCookieStickinessPolicy where
    xmlPickler = withNS elbNS

instance IsQuery CreatelbCookieStickinessPolicy

instance AWSRequest ELB CreatelbCookieStickinessPolicy CreatelbCookieStickinessPolicyResponse where
    request = req GET "CreatelbCookieStickinessPolicy"

data CreatelbCookieStickinessPolicyResponse = CreatelbCookieStickinessPolicyResponse
    { clbcsprResponseMetadata :: !ByteString
    , clbcsprCreatelbCookieStickinessPolicyResult :: !CreatelbCookieStickinessPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML CreatelbCookieStickinessPolicyResponse where
    xmlPickler = withNS elbNS

-- | Creates a new load balancer. After the call has completed successfully, a
-- new load balancer is created; however, it will not be usable until at least
-- one instance has been registered. When the load balancer creation is
-- completed, the client can check whether or not it is usable by using the
-- DescribeInstanceHealth action. The load balancer is usable as soon as any
-- registered instance is InService. Note Currently, the client's quota of
-- load balancers is limited to ten per Region. Note Load balancer DNS names
-- vary depending on the Region they're created in. For load balancers created
-- in the United States, the DNS name ends with: For load balancers created in
-- the EU (Ireland) Region, the DNS name ends with: For information on using
-- CreateLoadBalancer to create a new load balancer in Amazon EC2, go to Using
-- Query API section in the Creating a Load Balancer With SSL Cipher Settings
-- and Back-end Authentication topic of the Elastic Load Balancing Developer
-- Guide. For information on using CreateLoadBalancer to create a new load
-- balancer in Amazon VPC, see Using the Query API in the Creating a Basic
-- Load Balancer in Amazon VPC section in the Elastic Load Balancing Developer
-- Guide.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancer.html>
data CreateLoadBalancer = CreateLoadBalancer
    { clbAvailabilityZones :: Members ByteString
      -- ^ A list of Availability Zones.
    , clbListeners         :: Members Listener
      -- ^ A list of the following tuples: LoadBalancerPort, InstancePort,
      -- and Protocol.
    , clbLoadBalancerName  :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within your set of load balancers.
    , clbScheme            :: Maybe ByteString
      -- ^ The type of a load balancer.
    , clbSecurityGroups    :: Members ByteString
      -- ^ The security groups assigned to your load balancer within your
      -- VPC.
    , clbSubnets           :: Members ByteString
      -- ^ A list of subnet IDs in your VPC to attach to your load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoadBalancer where
    xmlPickler = withNS elbNS

instance IsQuery CreateLoadBalancer

instance AWSRequest ELB CreateLoadBalancer CreateLoadBalancerResponse where
    request = req GET "CreateLoadBalancer"

data CreateLoadBalancerResponse = CreateLoadBalancerResponse
    { clbrResponseMetadata :: !ByteString
    , clbrCreateLoadBalancerResult :: !CreateLoadBalancerResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoadBalancerResponse where
    xmlPickler = withNS elbNS

-- | Creates one or more listeners on a load balancer for the specified port. If
-- a listener with the given port does not already exist, it will be created;
-- otherwise, the properties of the new listener must match the properties of
-- the existing listener.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerListeners.html>
data CreateLoadBalancerListeners = CreateLoadBalancerListeners
    { clblListeners        :: Members Listener
      -- ^ A list of LoadBalancerPort, InstancePort, Protocol, and
      -- SSLCertificateId items.
    , clblLoadBalancerName :: !ByteString
      -- ^ The name of the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoadBalancerListeners where
    xmlPickler = withNS elbNS

instance IsQuery CreateLoadBalancerListeners

instance AWSRequest ELB CreateLoadBalancerListeners CreateLoadBalancerListenersResponse where
    request = req GET "CreateLoadBalancerListeners"

data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse
    { clblrResponseMetadata :: !ByteString
    , clblrCreateLoadBalancerListenersResult :: !CreateLoadBalancerListenersResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoadBalancerListenersResponse where
    xmlPickler = withNS elbNS

-- | Creates a new policy that contains the necessary attributes depending on
-- the policy type. Policies are settings that are saved for your load
-- balancer and that can be applied to the front-end listener, or the back-end
-- application server, depending on your policy type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerPolicy.html>
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy
    { clbpLoadBalancerName :: !ByteString
      -- ^ The name associated with the LoadBalancer for which the policy is
      -- being created. This name must be unique within the client AWS
      -- account.
    , clbpPolicyAttributes :: Members PolicyAttribute
      -- ^ A list of attributes associated with the policy being created.
    , clbpPolicyName       :: !ByteString
      -- ^ The name of the load balancer policy being created. The name must
      -- be unique within the set of policies for this load balancer.
    , clbpPolicyTypeName   :: !ByteString
      -- ^ The name of the base policy type being used to create this
      -- policy. To get the list of policy types, use the
      -- DescribeLoadBalancerPolicyTypes action.
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoadBalancerPolicy where
    xmlPickler = withNS elbNS

instance IsQuery CreateLoadBalancerPolicy

instance AWSRequest ELB CreateLoadBalancerPolicy CreateLoadBalancerPolicyResponse where
    request = req GET "CreateLoadBalancerPolicy"

data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse
    { clbprResponseMetadata :: !ByteString
    , clbprCreateLoadBalancerPolicyResult :: !CreateLoadBalancerPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoadBalancerPolicyResponse where
    xmlPickler = withNS elbNS

-- | Deletes the specified load balancer. If attempting to recreate the load
-- balancer, the client must reconfigure all the settings. The DNS name
-- associated with a deleted load balancer will no longer be usable. Once
-- deleted, the name and associated DNS record of the load balancer no longer
-- exist and traffic sent to any of its IP addresses will no longer be
-- delivered to client instances. The client will not receive the same DNS
-- name even if a new load balancer with same load balancer name is created.
-- To successfully call this API, the client must provide the same account
-- credentials as were used to create the load balancer. Note By design, if
-- the load balancer does not exist or has already been deleted,
-- DeleteLoadBalancer still succeeds.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancer.html>
data DeleteLoadBalancer = DeleteLoadBalancer
    { dlbLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteLoadBalancer where
    xmlPickler = withNS elbNS

instance IsQuery DeleteLoadBalancer

instance AWSRequest ELB DeleteLoadBalancer DeleteLoadBalancerResponse where
    request = req GET "DeleteLoadBalancer"

data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse
    { dlbrResponseMetadata :: !ByteString
    , dlbrDeleteLoadBalancerResult :: !DeleteLoadBalancerResult
    } deriving (Eq, Show, Generic)

instance IsXML DeleteLoadBalancerResponse where
    xmlPickler = withNS elbNS

-- | Deletes listeners from the load balancer for the specified port.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html>
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners
    { dlblLoadBalancerName  :: !ByteString
      -- ^ The mnemonic name associated with the load balancer.
    , dlblLoadBalancerPorts :: Members Integer
      -- ^ The client port number(s) of the load balancer listener(s) to be
      -- removed.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteLoadBalancerListeners where
    xmlPickler = withNS elbNS

instance IsQuery DeleteLoadBalancerListeners

instance AWSRequest ELB DeleteLoadBalancerListeners DeleteLoadBalancerListenersResponse where
    request = req GET "DeleteLoadBalancerListeners"

data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse
    { dlblrResponseMetadata :: !ByteString
    , dlblrDeleteLoadBalancerListenersResult :: !DeleteLoadBalancerListenersResult
    } deriving (Eq, Show, Generic)

instance IsXML DeleteLoadBalancerListenersResponse where
    xmlPickler = withNS elbNS

-- | Deletes a policy from the load balancer. The specified policy must not be
-- enabled for any listeners.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerPolicy.html>
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy
    { dlbpLoadBalancerName :: !ByteString
      -- ^ The mnemonic name associated with the load balancer. The name
      -- must be unique within your AWS account.
    , dlbpPolicyName       :: !ByteString
      -- ^ The mnemonic name for the policy being deleted.
    } deriving (Eq, Show, Generic)

instance IsXML DeleteLoadBalancerPolicy where
    xmlPickler = withNS elbNS

instance IsQuery DeleteLoadBalancerPolicy

instance AWSRequest ELB DeleteLoadBalancerPolicy DeleteLoadBalancerPolicyResponse where
    request = req GET "DeleteLoadBalancerPolicy"

data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse
    { dlbprResponseMetadata :: !ByteString
    , dlbprDeleteLoadBalancerPolicyResult :: !DeleteLoadBalancerPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML DeleteLoadBalancerPolicyResponse where
    xmlPickler = withNS elbNS

-- | Deregisters instances from the load balancer. Once the instance is
-- deregistered, it will stop receiving traffic from the load balancer. In
-- order to successfully call this API, the same account credentials as those
-- used to create the load balancer must be provided.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeregisterInstancesFromLoadBalancer.html>
data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer
    { diflbInstances        :: Members Instance
      -- ^ A list of EC2 instance IDs consisting of all instances to be
      -- deregistered.
    , diflbLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    } deriving (Eq, Show, Generic)

instance IsXML DeregisterInstancesFromLoadBalancer where
    xmlPickler = withNS elbNS

instance IsQuery DeregisterInstancesFromLoadBalancer

instance AWSRequest ELB DeregisterInstancesFromLoadBalancer DeregisterInstancesFromLoadBalancerResponse where
    request = req GET "DeregisterInstancesFromLoadBalancer"

data DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse
    { diflbrResponseMetadata :: !ByteString
    , diflbrDeregisterInstancesFromLoadBalancerResult :: !DeregisterInstancesFromLoadBalancerResult
    } deriving (Eq, Show, Generic)

instance IsXML DeregisterInstancesFromLoadBalancerResponse where
    xmlPickler = withNS elbNS

-- | Returns the current state of the instances of the specified load balancer.
-- If no instances are specified, the state of all the instances for the load
-- balancer is returned. Note The client must have created the specified input
-- load balancer in order to retrieve this information; the client must
-- provide the same account credentials as those that were used to create the
-- load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeInstanceHealth.html>
data DescribeInstanceHealth = DescribeInstanceHealth
    { dihInstances        :: Members Instance
      -- ^ A list of instance IDs whose states are being queried.
    , dihLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInstanceHealth where
    xmlPickler = withNS elbNS

instance IsQuery DescribeInstanceHealth

instance AWSRequest ELB DescribeInstanceHealth DescribeInstanceHealthResponse where
    request = req GET "DescribeInstanceHealth"

data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse
    { dihrResponseMetadata :: !ByteString
    , dihrDescribeInstanceHealthResult :: !DescribeInstanceHealthResult
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInstanceHealthResponse where
    xmlPickler = withNS elbNS

-- | Returns detailed descriptions of the policies. If you specify a load
-- balancer name, the operation returns either the descriptions of the
-- specified policies, or descriptions of all the policies created for the
-- load balancer. If you don't specify a load balancer name, the operation
-- returns descriptions of the specified sample policies, or descriptions of
-- all the sample policies. The names of the sample policies have the
-- ELBSample- prefix.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicies.html>
data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies
    { dlbqLoadBalancerName :: Maybe ByteString
      -- ^ The mnemonic name associated with the load balancer. If no name
      -- is specified, the operation returns the attributes of either all
      -- the sample policies pre-defined by Elastic Load Balancing or the
      -- specified sample polices.
    , dlbqPolicyNames      :: Members ByteString
      -- ^ The names of LoadBalancer policies you've created or Elastic Load
      -- Balancing sample policy names.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancerPolicies where
    xmlPickler = withNS elbNS

instance IsQuery DescribeLoadBalancerPolicies

instance AWSRequest ELB DescribeLoadBalancerPolicies DescribeLoadBalancerPoliciesResponse where
    request = req GET "DescribeLoadBalancerPolicies"

data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse
    { dlbpsResponseMetadata :: !ByteString
    , dlbpsDescribeLoadBalancerPoliciesResult :: !DescribeLoadBalancerPoliciesResult
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancerPoliciesResponse where
    xmlPickler = withNS elbNS

-- | Returns meta-information on the specified load balancer policies defined by
-- the Elastic Load Balancing service. The policy types that are returned from
-- this action can be used in a CreateLoadBalancerPolicy action to instantiate
-- specific policy configurations that will be applied to a load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicyTypes.html>
data DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes
    { dlbptPolicyTypeNames :: Members ByteString
      -- ^ Specifies the name of the policy types. If no names are
      -- specified, returns the description of all the policy types
      -- defined by Elastic Load Balancing service.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancerPolicyTypes where
    xmlPickler = withNS elbNS

instance IsQuery DescribeLoadBalancerPolicyTypes

instance AWSRequest ELB DescribeLoadBalancerPolicyTypes DescribeLoadBalancerPolicyTypesResponse where
    request = req GET "DescribeLoadBalancerPolicyTypes"

data DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse
    { dlbptrResponseMetadata :: !ByteString
    , dlbptrDescribeLoadBalancerPolicyTypesResult :: !DescribeLoadBalancerPolicyTypesResult
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancerPolicyTypesResponse where
    xmlPickler = withNS elbNS

-- | Returns detailed configuration information for the specified load
-- balancers. If no load balancers are specified, the operation returns
-- configuration information for all load balancers created by the caller.
-- Note The client must have created the specified input load balancers in
-- order to retrieve this information; the client must provide the same
-- account credentials as those that were used to create the load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancers.html>
data DescribeLoadBalancers = DescribeLoadBalancers
    { dlbLoadBalancerNames :: Members ByteString
      -- ^ A list of names associated with the load balancers at creation
      -- time.
    , dlbMarker            :: Maybe ByteString
      -- ^ An optional parameter reserved for future use.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancers where
    xmlPickler = withNS elbNS

instance IsQuery DescribeLoadBalancers

instance AWSRequest ELB DescribeLoadBalancers DescribeLoadBalancersResponse where
    request = req GET "DescribeLoadBalancers"

data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { dlbsResponseMetadata :: !ByteString
    , dlbsDescribeLoadBalancersResult :: !DescribeLoadBalancersResult
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancersResponse where
    xmlPickler = withNS elbNS

-- | Removes subnets from the set of configured subnets in the VPC for the load
-- balancer. After a subnet is removed all of the EndPoints registered with
-- the load balancer that are in the removed subnet will go into the
-- OutOfService state. When a subnet is removed, the load balancer will
-- balance the traffic among the remaining routable subnets for the load
-- balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DetachLoadBalancerFromSubnets.html>
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets
    { dlbfsLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer to be detached. The
      -- name must be unique within the client AWS account.
    , dlbfsSubnets          :: Members ByteString
      -- ^ A list of subnet IDs to remove from the set of configured subnets
      -- for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML DetachLoadBalancerFromSubnets where
    xmlPickler = withNS elbNS

instance IsQuery DetachLoadBalancerFromSubnets

instance AWSRequest ELB DetachLoadBalancerFromSubnets DetachLoadBalancerFromSubnetsResponse where
    request = req GET "DetachLoadBalancerFromSubnets"

data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { dlbfsrResponseMetadata :: !ByteString
    , dlbfsrDetachLoadBalancerFromSubnetsResult :: !DetachLoadBalancerFromSubnetsResult
    } deriving (Eq, Show, Generic)

instance IsXML DetachLoadBalancerFromSubnetsResponse where
    xmlPickler = withNS elbNS

-- | Removes the specified EC2 Availability Zones from the set of configured
-- Availability Zones for the load balancer. There must be at least one
-- Availability Zone registered with a load balancer at all times. A client
-- cannot remove all the Availability Zones from a load balancer. Once an
-- Availability Zone is removed, all the instances registered with the load
-- balancer that are in the removed Availability Zone go into the OutOfService
-- state. Upon Availability Zone removal, the load balancer attempts to
-- equally balance the traffic among its remaining usable Availability Zones.
-- Trying to remove an Availability Zone that was not associated with the load
-- balancer does nothing. Note In order for this call to be successful, the
-- client must have created the load balancer. The client must provide the
-- same account credentials as those that were used to create the load
-- balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DisableAvailabilityZonesForLoadBalancer.html>
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer
    { dazflbAvailabilityZones :: Members ByteString
      -- ^ A list of Availability Zones to be removed from the load
      -- balancer.
    , dazflbLoadBalancerName  :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    } deriving (Eq, Show, Generic)

instance IsXML DisableAvailabilityZonesForLoadBalancer where
    xmlPickler = withNS elbNS

instance IsQuery DisableAvailabilityZonesForLoadBalancer

instance AWSRequest ELB DisableAvailabilityZonesForLoadBalancer DisableAvailabilityZonesForLoadBalancerResponse where
    request = req GET "DisableAvailabilityZonesForLoadBalancer"

data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse
    { dazflbrResponseMetadata :: !ByteString
    , dazflbrDisableAvailabilityZonesForLoadBalancerResult :: !DisableAvailabilityZonesForLoadBalancerResult
    } deriving (Eq, Show, Generic)

instance IsXML DisableAvailabilityZonesForLoadBalancerResponse where
    xmlPickler = withNS elbNS

-- | Adds one or more EC2 Availability Zones to the load balancer. The load
-- balancer evenly distributes requests across all its registered Availability
-- Zones that contain instances. As a result, the client must ensure that its
-- load balancer is appropriately scaled for each registered Availability
-- Zone. Note The new EC2 Availability Zones to be added must be in the same
-- EC2 Region as the Availability Zones for which the load balancer was
-- created.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_EnableAvailabilityZonesForLoadBalancer.html>
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer
    { eazflbAvailabilityZones :: Members ByteString
      -- ^ A list of new Availability Zones for the load balancer. Each
      -- Availability Zone must be in the same Region as the load
      -- balancer.
    , eazflbLoadBalancerName  :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    } deriving (Eq, Show, Generic)

instance IsXML EnableAvailabilityZonesForLoadBalancer where
    xmlPickler = withNS elbNS

instance IsQuery EnableAvailabilityZonesForLoadBalancer

instance AWSRequest ELB EnableAvailabilityZonesForLoadBalancer EnableAvailabilityZonesForLoadBalancerResponse where
    request = req GET "EnableAvailabilityZonesForLoadBalancer"

data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse
    { eazflbrResponseMetadata :: !ByteString
    , eazflbrEnableAvailabilityZonesForLoadBalancerResult :: !EnableAvailabilityZonesForLoadBalancerResult
    } deriving (Eq, Show, Generic)

instance IsXML EnableAvailabilityZonesForLoadBalancerResponse where
    xmlPickler = withNS elbNS

-- | Adds new instances to the load balancer. Once the instance is registered,
-- it starts receiving traffic and requests from the load balancer. Any
-- instance that is not in any of the Availability Zones registered for the
-- load balancer will be moved to the OutOfService state. It will move to the
-- InService state when the Availability Zone is added to the load balancer.
-- Note In order for this call to be successful, the client must have created
-- the load balancer. The client must provide the same account credentials as
-- those that were used to create the load balancer. Note Completion of this
-- API does not guarantee that operation has completed. Rather, it means that
-- the request has been registered and the changes will happen shortly.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_RegisterInstancesWithLoadBalancer.html>
data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer
    { riwlbInstances        :: Members Instance
      -- ^ A list of instance IDs that should be registered with the load
      -- balancer.
    , riwlbLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    } deriving (Eq, Show, Generic)

instance IsXML RegisterInstancesWithLoadBalancer where
    xmlPickler = withNS elbNS

instance IsQuery RegisterInstancesWithLoadBalancer

instance AWSRequest ELB RegisterInstancesWithLoadBalancer RegisterInstancesWithLoadBalancerResponse where
    request = req GET "RegisterInstancesWithLoadBalancer"

data RegisterInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse
    { riwlbrResponseMetadata :: !ByteString
    , riwlbrRegisterInstancesWithLoadBalancerResult :: !RegisterInstancesWithLoadBalancerResult
    } deriving (Eq, Show, Generic)

instance IsXML RegisterInstancesWithLoadBalancerResponse where
    xmlPickler = withNS elbNS

-- | Sets the certificate that terminates the specified listener's SSL
-- connections. The specified certificate replaces any prior certificate that
-- was used on the same load balancer and port. For information on using
-- SetLoadBalancerListenerSSLCertificate, see Using the Query API in the
-- Updating an SSL Certificate for a Load Balancer section in of the Elastic
-- Load Balancing Developer Guide.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerListenerSSLCertificate.html>
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate
    { slblsslcLoadBalancerName :: !ByteString
      -- ^ The name of the the load balancer.
    , slblsslcLoadBalancerPort :: !Integer
      -- ^ The port that uses the specified SSL certificate.
    , slblsslcSSLCertificateId :: !ByteString
      -- ^ The ID of the SSL certificate chain to use. For more information
      -- on SSL certificates, see Managing Server Certificates in the AWS
      -- Identity and Access Management documentation.
    } deriving (Eq, Show, Generic)

instance IsXML SetLoadBalancerListenerSSLCertificate where
    xmlPickler = withNS elbNS

instance IsQuery SetLoadBalancerListenerSSLCertificate

instance AWSRequest ELB SetLoadBalancerListenerSSLCertificate SetLoadBalancerListenerSSLCertificateResponse where
    request = req GET "SetLoadBalancerListenerSSLCertificate"

data SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse
    { slblsslcrResponseMetadata :: !ByteString
    , slblsslcrSetLoadBalancerListenerSSLCertificateResult :: !SetLoadBalancerListenerSSLCertificateResult
    } deriving (Eq, Show, Generic)

instance IsXML SetLoadBalancerListenerSSLCertificateResponse where
    xmlPickler = withNS elbNS

-- | Replaces the current set of policies associated with a port on which the
-- back-end server is listening with a new set of policies. After the policies
-- have been created using CreateLoadBalancerPolicy, they can be applied here
-- as a list. At this time, only the back-end server authentication policy
-- type can be applied to the back-end ports; this policy type is composed of
-- multiple public key policies.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerPoliciesForBackendServer.html>
data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer
    { slbpfbsInstancePort     :: !Integer
      -- ^ The port number associated with the back-end server.
    , slbpfbsLoadBalancerName :: !ByteString
      -- ^ The mnemonic name associated with the load balancer. This name
      -- must be unique within the client AWS account.
    , slbpfbsPolicyNames      :: Members ByteString
      -- ^ List of policy names to be set. If the list is empty, then all
      -- current polices are removed from the back-end server.
    } deriving (Eq, Show, Generic)

instance IsXML SetLoadBalancerPoliciesForBackendServer where
    xmlPickler = withNS elbNS

instance IsQuery SetLoadBalancerPoliciesForBackendServer

instance AWSRequest ELB SetLoadBalancerPoliciesForBackendServer SetLoadBalancerPoliciesForBackendServerResponse where
    request = req GET "SetLoadBalancerPoliciesForBackendServer"

data SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse
    { slbpfbsrResponseMetadata :: !ByteString
    , slbpfbsrSetLoadBalancerPoliciesForBackendServerResult :: !SetLoadBalancerPoliciesForBackendServerResult
    } deriving (Eq, Show, Generic)

instance IsXML SetLoadBalancerPoliciesForBackendServerResponse where
    xmlPickler = withNS elbNS

-- | Associates, updates, or disables a policy with a listener on the load
-- balancer. You can associate multiple policies with a listener.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SetLoadBalancerPoliciesOfListener.html>
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener
    { slbpolLoadBalancerName :: !ByteString
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the client AWS account.
    , slbpolLoadBalancerPort :: !Integer
      -- ^ The external port of the load balancer with which this policy
      -- applies to.
    , slbpolPolicyNames      :: Members ByteString
      -- ^ List of policies to be associated with the listener. Currently
      -- this list can have at most one policy. If the list is empty, the
      -- current policy is removed from the listener.
    } deriving (Eq, Show, Generic)

instance IsXML SetLoadBalancerPoliciesOfListener where
    xmlPickler = withNS elbNS

instance IsQuery SetLoadBalancerPoliciesOfListener

instance AWSRequest ELB SetLoadBalancerPoliciesOfListener SetLoadBalancerPoliciesOfListenerResponse where
    request = req GET "SetLoadBalancerPoliciesOfListener"

data SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse
    { slbpolrResponseMetadata :: !ByteString
    , slbpolrSetLoadBalancerPoliciesOfListenerResult :: !SetLoadBalancerPoliciesOfListenerResult
    } deriving (Eq, Show, Generic)

instance IsXML SetLoadBalancerPoliciesOfListenerResponse where
    xmlPickler = withNS elbNS
