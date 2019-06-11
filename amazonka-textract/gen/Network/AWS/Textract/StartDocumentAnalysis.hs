{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract.StartDocumentAnalysis
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous analysis of an input document for relationships between detected items such as key and value pairs, tables, and selection elements.
--
--
-- @StartDocumentAnalysis@ can analyze text in documents that are in JPG, PNG, and PDF format. The documents are stored in an Amazon S3 bucket. Use 'DocumentLocation' to specify the bucket name and file name of the document.
--
-- @StartDocumentAnalysis@ returns a job identifier (@JobId@ ) that you use to get the results of the operation. When text analysis is finished, Amazon Textract publishes a completion status to the Amazon Simple Notification Service (Amazon SNS) topic that you specify in @NotificationChannel@ . To get the results of the text analysis operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetDocumentAnalysis' , and pass the job identifier (@JobId@ ) from the initial call to @StartDocumentAnalysis@ .
--
-- For more information, see <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-analyzing.html Document Text Analysis> .
--
module Network.AWS.Textract.StartDocumentAnalysis
    (
    -- * Creating a Request
      startDocumentAnalysis
    , StartDocumentAnalysis
    -- * Request Lenses
    , sdaJobTag
    , sdaNotificationChannel
    , sdaClientRequestToken
    , sdaDocumentLocation
    , sdaFeatureTypes

    -- * Destructuring the Response
    , startDocumentAnalysisResponse
    , StartDocumentAnalysisResponse
    -- * Response Lenses
    , sdarsJobId
    , sdarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Textract.Types
import Network.AWS.Textract.Types.Product

-- | /See:/ 'startDocumentAnalysis' smart constructor.
data StartDocumentAnalysis = StartDocumentAnalysis'
  { _sdaJobTag              :: !(Maybe Text)
  , _sdaNotificationChannel :: !(Maybe NotificationChannel)
  , _sdaClientRequestToken  :: !(Maybe Text)
  , _sdaDocumentLocation    :: !DocumentLocation
  , _sdaFeatureTypes        :: ![FeatureType]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDocumentAnalysis' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdaJobTag' - An identifier you specify that's included in the completion notification that's published to the Amazon SNS topic. For example, you can use @JobTag@ to identify the type of document, such as a tax form or a receipt, that the completion notification corresponds to.
--
-- * 'sdaNotificationChannel' - The Amazon SNS topic ARN that you want Amazon Textract to publish the completion status of the operation to.
--
-- * 'sdaClientRequestToken' - The idempotent token that you use to identify the start request. If you use the same token with multiple @StartDocumentAnalysis@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentally started more than once.
--
-- * 'sdaDocumentLocation' - The location of the document to be processed.
--
-- * 'sdaFeatureTypes' - A list of the types of analysis to perform. Add TABLES to the list to return information about the tables that are detected in the input document. Add FORMS to return detected fields and the associated text. To perform both types of analysis, add TABLES and FORMS to @FeatureTypes@ . All selectable elements (@SELECTION_ELEMENT@ ) that are detected are returned, whatever the value of @FeatureTypes@ .
startDocumentAnalysis
    :: DocumentLocation -- ^ 'sdaDocumentLocation'
    -> StartDocumentAnalysis
startDocumentAnalysis pDocumentLocation_ =
  StartDocumentAnalysis'
    { _sdaJobTag = Nothing
    , _sdaNotificationChannel = Nothing
    , _sdaClientRequestToken = Nothing
    , _sdaDocumentLocation = pDocumentLocation_
    , _sdaFeatureTypes = mempty
    }


-- | An identifier you specify that's included in the completion notification that's published to the Amazon SNS topic. For example, you can use @JobTag@ to identify the type of document, such as a tax form or a receipt, that the completion notification corresponds to.
sdaJobTag :: Lens' StartDocumentAnalysis (Maybe Text)
sdaJobTag = lens _sdaJobTag (\ s a -> s{_sdaJobTag = a})

-- | The Amazon SNS topic ARN that you want Amazon Textract to publish the completion status of the operation to.
sdaNotificationChannel :: Lens' StartDocumentAnalysis (Maybe NotificationChannel)
sdaNotificationChannel = lens _sdaNotificationChannel (\ s a -> s{_sdaNotificationChannel = a})

-- | The idempotent token that you use to identify the start request. If you use the same token with multiple @StartDocumentAnalysis@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentally started more than once.
sdaClientRequestToken :: Lens' StartDocumentAnalysis (Maybe Text)
sdaClientRequestToken = lens _sdaClientRequestToken (\ s a -> s{_sdaClientRequestToken = a})

-- | The location of the document to be processed.
sdaDocumentLocation :: Lens' StartDocumentAnalysis DocumentLocation
sdaDocumentLocation = lens _sdaDocumentLocation (\ s a -> s{_sdaDocumentLocation = a})

-- | A list of the types of analysis to perform. Add TABLES to the list to return information about the tables that are detected in the input document. Add FORMS to return detected fields and the associated text. To perform both types of analysis, add TABLES and FORMS to @FeatureTypes@ . All selectable elements (@SELECTION_ELEMENT@ ) that are detected are returned, whatever the value of @FeatureTypes@ .
sdaFeatureTypes :: Lens' StartDocumentAnalysis [FeatureType]
sdaFeatureTypes = lens _sdaFeatureTypes (\ s a -> s{_sdaFeatureTypes = a}) . _Coerce

instance AWSRequest StartDocumentAnalysis where
        type Rs StartDocumentAnalysis =
             StartDocumentAnalysisResponse
        request = postJSON textract
        response
          = receiveJSON
              (\ s h x ->
                 StartDocumentAnalysisResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartDocumentAnalysis where

instance NFData StartDocumentAnalysis where

instance ToHeaders StartDocumentAnalysis where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Textract.StartDocumentAnalysis" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartDocumentAnalysis where
        toJSON StartDocumentAnalysis'{..}
          = object
              (catMaybes
                 [("JobTag" .=) <$> _sdaJobTag,
                  ("NotificationChannel" .=) <$>
                    _sdaNotificationChannel,
                  ("ClientRequestToken" .=) <$> _sdaClientRequestToken,
                  Just ("DocumentLocation" .= _sdaDocumentLocation),
                  Just ("FeatureTypes" .= _sdaFeatureTypes)])

instance ToPath StartDocumentAnalysis where
        toPath = const "/"

instance ToQuery StartDocumentAnalysis where
        toQuery = const mempty

-- | /See:/ 'startDocumentAnalysisResponse' smart constructor.
data StartDocumentAnalysisResponse = StartDocumentAnalysisResponse'
  { _sdarsJobId          :: !(Maybe Text)
  , _sdarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDocumentAnalysisResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdarsJobId' - The identifier for the document text detection job. Use @JobId@ to identify the job in a subsequent call to @GetDocumentAnalysis@ .
--
-- * 'sdarsResponseStatus' - -- | The response status code.
startDocumentAnalysisResponse
    :: Int -- ^ 'sdarsResponseStatus'
    -> StartDocumentAnalysisResponse
startDocumentAnalysisResponse pResponseStatus_ =
  StartDocumentAnalysisResponse'
    {_sdarsJobId = Nothing, _sdarsResponseStatus = pResponseStatus_}


-- | The identifier for the document text detection job. Use @JobId@ to identify the job in a subsequent call to @GetDocumentAnalysis@ .
sdarsJobId :: Lens' StartDocumentAnalysisResponse (Maybe Text)
sdarsJobId = lens _sdarsJobId (\ s a -> s{_sdarsJobId = a})

-- | -- | The response status code.
sdarsResponseStatus :: Lens' StartDocumentAnalysisResponse Int
sdarsResponseStatus = lens _sdarsResponseStatus (\ s a -> s{_sdarsResponseStatus = a})

instance NFData StartDocumentAnalysisResponse where
