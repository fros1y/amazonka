{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CognitoSync.DescribeIdentityUsage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets usage information for an identity, including number of datasets and
-- data usage.
module Network.AWS.CognitoSync.DescribeIdentityUsage
    (
    -- * Request
      DescribeIdentityUsage
    -- ** Request constructor
    , describeIdentityUsage
    -- ** Request lenses
    , diuIdentityId
    , diuIdentityPoolId

    -- * Response
    , DescribeIdentityUsageResponse
    -- ** Response constructor
    , describeIdentityUsageResponse
    -- ** Response lenses
    , diurIdentityUsage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoSync.Types

data DescribeIdentityUsage = DescribeIdentityUsage
    { _diuIdentityId     :: Text
    , _diuIdentityPoolId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeIdentityUsage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diuIdentityId' @::@ 'Text'
--
-- * 'diuIdentityPoolId' @::@ 'Text'
--
describeIdentityUsage :: Text -- ^ 'diuIdentityPoolId'
                      -> Text -- ^ 'diuIdentityId'
                      -> DescribeIdentityUsage
describeIdentityUsage p1 p2 = DescribeIdentityUsage
    { _diuIdentityPoolId = p1
    , _diuIdentityId     = p2
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
diuIdentityId :: Lens' DescribeIdentityUsage Text
diuIdentityId = lens _diuIdentityId (\s a -> s { _diuIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
diuIdentityPoolId :: Lens' DescribeIdentityUsage Text
diuIdentityPoolId =
    lens _diuIdentityPoolId (\s a -> s { _diuIdentityPoolId = a })

instance ToPath DescribeIdentityUsage where
    toPath DescribeIdentityUsage{..} = mconcat
        [ "/identitypools/"
        , toText _diuIdentityPoolId
        , "/identities/"
        , toText _diuIdentityId
        ]

instance ToQuery DescribeIdentityUsage where
    toQuery = const mempty

instance ToHeaders DescribeIdentityUsage

newtype DescribeIdentityUsageResponse = DescribeIdentityUsageResponse
    { _diurIdentityUsage :: Maybe IdentityUsage
    } deriving (Eq, Show, Generic)

-- | 'DescribeIdentityUsageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diurIdentityUsage' @::@ 'Maybe' 'IdentityUsage'
--
describeIdentityUsageResponse :: DescribeIdentityUsageResponse
describeIdentityUsageResponse = DescribeIdentityUsageResponse
    { _diurIdentityUsage = Nothing
    }

-- | Usage information for the identity.
diurIdentityUsage :: Lens' DescribeIdentityUsageResponse (Maybe IdentityUsage)
diurIdentityUsage =
    lens _diurIdentityUsage (\s a -> s { _diurIdentityUsage = a })

-- FromJSON

instance AWSRequest DescribeIdentityUsage where
    type Sv DescribeIdentityUsage = CognitoSync
    type Rs DescribeIdentityUsage = DescribeIdentityUsageResponse

    request  = get'
    response = jsonResponse $ \h o -> DescribeIdentityUsageResponse
        <$> o .: "IdentityUsage"
