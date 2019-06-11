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
-- Module      : Network.AWS.Textract.StartDocumentTextDetection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous detection of text in a document. Amazon Textract can detect lines of text and the words that make up a line of text.
--
--
-- @StartDocumentTextDetection@ can analyze text in documents that are in JPG, PNG, and PDF format. The documents are stored in an Amazon S3 bucket. Use 'DocumentLocation' to specify the bucket name and file name of the document.
--
-- @StartTextDetection@ returns a job identifier (@JobId@ ) that you use to get the results of the operation. When text detection is finished, Amazon Textract publishes a completion status to the Amazon Simple Notification Service (Amazon SNS) topic that you specify in @NotificationChannel@ . To get the results of the text detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetDocumentTextDetection' , and pass the job identifier (@JobId@ ) from the initial call to @StartDocumentTextDetection@ .
--
-- For more information, see <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-detecting.html Document Text Detection> .
--
module Network.AWS.Textract.StartDocumentTextDetection
    (
    -- * Creating a Request
      startDocumentTextDetection
    , StartDocumentTextDetection
    -- * Request Lenses
    , sdtdJobTag
    , sdtdNotificationChannel
    , sdtdClientRequestToken
    , sdtdDocumentLocation

    -- * Destructuring the Response
    , startDocumentTextDetectionResponse
    , StartDocumentTextDetectionResponse
    -- * Response Lenses
    , sdtdrsJobId
    , sdtdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Textract.Types
import Network.AWS.Textract.Types.Product

-- | /See:/ 'startDocumentTextDetection' smart constructor.
data StartDocumentTextDetection = StartDocumentTextDetection'
  { _sdtdJobTag              :: !(Maybe Text)
  , _sdtdNotificationChannel :: !(Maybe NotificationChannel)
  , _sdtdClientRequestToken  :: !(Maybe Text)
  , _sdtdDocumentLocation    :: !DocumentLocation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDocumentTextDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdtdJobTag' - An identifier you specify that's included in the completion notification that's published to the Amazon SNS topic. For example, you can use @JobTag@ to identify the type of document, such as a tax form or a receipt, that the completion notification corresponds to.
--
-- * 'sdtdNotificationChannel' - The Amazon SNS topic ARN that you want Amazon Textract to publish the completion status of the operation to.
--
-- * 'sdtdClientRequestToken' - The idempotent token that's used to identify the start request. If you use the same token with multiple @StartDocumentTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentally started more than once.
--
-- * 'sdtdDocumentLocation' - The location of the document to be processed.
startDocumentTextDetection
    :: DocumentLocation -- ^ 'sdtdDocumentLocation'
    -> StartDocumentTextDetection
startDocumentTextDetection pDocumentLocation_ =
  StartDocumentTextDetection'
    { _sdtdJobTag = Nothing
    , _sdtdNotificationChannel = Nothing
    , _sdtdClientRequestToken = Nothing
    , _sdtdDocumentLocation = pDocumentLocation_
    }


-- | An identifier you specify that's included in the completion notification that's published to the Amazon SNS topic. For example, you can use @JobTag@ to identify the type of document, such as a tax form or a receipt, that the completion notification corresponds to.
sdtdJobTag :: Lens' StartDocumentTextDetection (Maybe Text)
sdtdJobTag = lens _sdtdJobTag (\ s a -> s{_sdtdJobTag = a})

-- | The Amazon SNS topic ARN that you want Amazon Textract to publish the completion status of the operation to.
sdtdNotificationChannel :: Lens' StartDocumentTextDetection (Maybe NotificationChannel)
sdtdNotificationChannel = lens _sdtdNotificationChannel (\ s a -> s{_sdtdNotificationChannel = a})

-- | The idempotent token that's used to identify the start request. If you use the same token with multiple @StartDocumentTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentally started more than once.
sdtdClientRequestToken :: Lens' StartDocumentTextDetection (Maybe Text)
sdtdClientRequestToken = lens _sdtdClientRequestToken (\ s a -> s{_sdtdClientRequestToken = a})

-- | The location of the document to be processed.
sdtdDocumentLocation :: Lens' StartDocumentTextDetection DocumentLocation
sdtdDocumentLocation = lens _sdtdDocumentLocation (\ s a -> s{_sdtdDocumentLocation = a})

instance AWSRequest StartDocumentTextDetection where
        type Rs StartDocumentTextDetection =
             StartDocumentTextDetectionResponse
        request = postJSON textract
        response
          = receiveJSON
              (\ s h x ->
                 StartDocumentTextDetectionResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartDocumentTextDetection where

instance NFData StartDocumentTextDetection where

instance ToHeaders StartDocumentTextDetection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Textract.StartDocumentTextDetection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartDocumentTextDetection where
        toJSON StartDocumentTextDetection'{..}
          = object
              (catMaybes
                 [("JobTag" .=) <$> _sdtdJobTag,
                  ("NotificationChannel" .=) <$>
                    _sdtdNotificationChannel,
                  ("ClientRequestToken" .=) <$>
                    _sdtdClientRequestToken,
                  Just ("DocumentLocation" .= _sdtdDocumentLocation)])

instance ToPath StartDocumentTextDetection where
        toPath = const "/"

instance ToQuery StartDocumentTextDetection where
        toQuery = const mempty

-- | /See:/ 'startDocumentTextDetectionResponse' smart constructor.
data StartDocumentTextDetectionResponse = StartDocumentTextDetectionResponse'
  { _sdtdrsJobId          :: !(Maybe Text)
  , _sdtdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDocumentTextDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdtdrsJobId' - The identifier for the document text-detection job. Use @JobId@ to identify the job in a subsequent call to @GetDocumentTextDetection@ .
--
-- * 'sdtdrsResponseStatus' - -- | The response status code.
startDocumentTextDetectionResponse
    :: Int -- ^ 'sdtdrsResponseStatus'
    -> StartDocumentTextDetectionResponse
startDocumentTextDetectionResponse pResponseStatus_ =
  StartDocumentTextDetectionResponse'
    {_sdtdrsJobId = Nothing, _sdtdrsResponseStatus = pResponseStatus_}


-- | The identifier for the document text-detection job. Use @JobId@ to identify the job in a subsequent call to @GetDocumentTextDetection@ .
sdtdrsJobId :: Lens' StartDocumentTextDetectionResponse (Maybe Text)
sdtdrsJobId = lens _sdtdrsJobId (\ s a -> s{_sdtdrsJobId = a})

-- | -- | The response status code.
sdtdrsResponseStatus :: Lens' StartDocumentTextDetectionResponse Int
sdtdrsResponseStatus = lens _sdtdrsResponseStatus (\ s a -> s{_sdtdrsResponseStatus = a})

instance NFData StartDocumentTextDetectionResponse
         where
