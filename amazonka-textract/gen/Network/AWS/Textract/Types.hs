{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Textract.Types
    (
    -- * Service Configuration
      textract

    -- * Errors
    , _AccessDeniedException
    , _BadDocumentException
    , _InvalidParameterException
    , _UnsupportedDocumentException
    , _InvalidS3ObjectException
    , _ProvisionedThroughputExceededException
    , _ThrottlingException
    , _InternalServerError
    , _DocumentTooLargeException

    -- * BlockType
    , BlockType (..)

    -- * RelationshipType
    , RelationshipType (..)

    -- * Block
    , Block
    , block
    , bText
    , bConfidence
    , bRelationships
    , bGeometry
    , bId
    , bBlockType

    -- * BoundingBox
    , BoundingBox
    , boundingBox
    , bbHeight
    , bbLeft
    , bbWidth
    , bbTop

    -- * Document
    , Document
    , document
    , dS3Object
    , dBytes

    -- * DocumentMetadata
    , DocumentMetadata
    , documentMetadata
    , dmPages

    -- * Geometry
    , Geometry
    , geometry
    , gBoundingBox
    , gPolygon

    -- * Point
    , Point
    , point
    , pX
    , pY

    -- * Relationship
    , Relationship
    , relationship
    , rIds
    , rType

    -- * S3Object
    , S3Object
    , s3Object
    , soBucket
    , soName
    , soVersion
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Textract.Types.Product
import Network.AWS.Textract.Types.Sum

-- | API version @2019-06-03@ of the Amazon Textract SDK configuration.
textract :: Service
textract =
  Service
    { _svcAbbrev = "Textract"
    , _svcSigner = v4
    , _svcPrefix = "textract"
    , _svcVersion = "2019-06-03"
    , _svcEndpoint = defaultEndpoint textract
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Textract"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | You are not authorized to perform the action.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _MatchServiceError textract "AccessDeniedException"


-- | Amazon Textract isn't able to read the document
--
--
_BadDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_BadDocumentException = _MatchServiceError textract "BadDocumentException"


-- | Input parameter violated a constraint. Validate your parameter before calling the API operation again.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError textract "InvalidParameterException"


-- | The format of the input document isn't supported. Documents for synchronous operations can be in PNG or JPEG format. Documents for asynchronous operations can also be in PDF format.
--
--
_UnsupportedDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedDocumentException =
  _MatchServiceError textract "UnsupportedDocumentException"


-- | Amazon Rekognition is unable to access the S3 object specified in the request.
--
--
_InvalidS3ObjectException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3ObjectException =
  _MatchServiceError textract "InvalidS3ObjectException"


-- | The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Rekognition.
--
--
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException =
  _MatchServiceError textract "ProvisionedThroughputExceededException"


-- | Amazon Rekognition is temporarily unable to process the request. Try your call again.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException = _MatchServiceError textract "ThrottlingException"


-- | Amazon Rekognition experienced a service issue. Try your call again.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError textract "InternalServerError"


-- | The document size exceeds the allowed limit. For more information, see 'limits' .
--
--
_DocumentTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentTooLargeException =
  _MatchServiceError textract "DocumentTooLargeException"

