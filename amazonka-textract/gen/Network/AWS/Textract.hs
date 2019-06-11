{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Textract detects and analyzes text in documents and converts it into machine-readable text. This is the API reference documentation for Amazon Textract.
--
--
module Network.AWS.Textract
    (
    -- * Service Configuration
      textract

    -- * Errors
    -- $errors

    -- ** InvalidJobIdException
    , _InvalidJobIdException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** BadDocumentException
    , _BadDocumentException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** UnsupportedDocumentException
    , _UnsupportedDocumentException

    -- ** InvalidS3ObjectException
    , _InvalidS3ObjectException

    -- ** ProvisionedThroughputExceededException
    , _ProvisionedThroughputExceededException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalServerError
    , _InternalServerError

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** DocumentTooLargeException
    , _DocumentTooLargeException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DetectDocumentText
    , module Network.AWS.Textract.DetectDocumentText

    -- ** StartDocumentAnalysis
    , module Network.AWS.Textract.StartDocumentAnalysis

    -- ** AnalyzeDocument
    , module Network.AWS.Textract.AnalyzeDocument

    -- ** GetDocumentTextDetection
    , module Network.AWS.Textract.GetDocumentTextDetection

    -- ** StartDocumentTextDetection
    , module Network.AWS.Textract.StartDocumentTextDetection

    -- ** GetDocumentAnalysis
    , module Network.AWS.Textract.GetDocumentAnalysis

    -- * Types

    -- ** BlockType
    , BlockType (..)

    -- ** EntityType
    , EntityType (..)

    -- ** FeatureType
    , FeatureType (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** RelationshipType
    , RelationshipType (..)

    -- ** SelectionStatus
    , SelectionStatus (..)

    -- ** Block
    , Block
    , block
    , bColumnSpan
    , bText
    , bEntityTypes
    , bColumnIndex
    , bPage
    , bRowSpan
    , bSelectionStatus
    , bRowIndex
    , bConfidence
    , bRelationships
    , bGeometry
    , bId
    , bBlockType

    -- ** BoundingBox
    , BoundingBox
    , boundingBox
    , bbHeight
    , bbLeft
    , bbWidth
    , bbTop

    -- ** Document
    , Document
    , document
    , dS3Object
    , dBytes

    -- ** DocumentLocation
    , DocumentLocation
    , documentLocation
    , dlS3Object

    -- ** DocumentMetadata
    , DocumentMetadata
    , documentMetadata
    , dmPages

    -- ** Geometry
    , Geometry
    , geometry
    , gBoundingBox
    , gPolygon

    -- ** NotificationChannel
    , NotificationChannel
    , notificationChannel
    , ncSNSTopicARN
    , ncRoleARN

    -- ** Point
    , Point
    , point
    , pX
    , pY

    -- ** Relationship
    , Relationship
    , relationship
    , rIds
    , rType

    -- ** S3Object
    , S3Object
    , s3Object
    , soBucket
    , soName
    , soVersion

    -- ** Warning
    , Warning
    , warning
    , wPages
    , wErrorCode
    ) where

import Network.AWS.Textract.AnalyzeDocument
import Network.AWS.Textract.DetectDocumentText
import Network.AWS.Textract.GetDocumentAnalysis
import Network.AWS.Textract.GetDocumentTextDetection
import Network.AWS.Textract.StartDocumentAnalysis
import Network.AWS.Textract.StartDocumentTextDetection
import Network.AWS.Textract.Types
import Network.AWS.Textract.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Textract'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
