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
-- Module      : Network.AWS.Textract.GetDocumentTextDetection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the results for an Amazon Textract asynchronous operation that detects text in a document. Amazon Textract can detect lines of text and the words that make up a line of text.
--
--
-- You start asynchronous text detection by calling 'StartDocumentTextDetection' , which returns a job identifier (@JobId@ ). When the text detection operation finishes, Amazon Textract publishes a completion status to the Amazon Simple Notification Service (Amazon SNS) topic that's registered in the initial call to @StartDocumentTextDetection@ . To get the results of the text-detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetDocumentTextDetection@ , and pass the job identifier (@JobId@ ) from the initial call to @StartDocumentTextDetection@ .
--
-- @GetDocumentTextDetection@ returns an array of 'Block' objects.
--
-- Each document page has as an associated @Block@ of type PAGE. Each PAGE @Block@ object is the parent of LINE @Block@ objects that represent the lines of detected text on a page. A LINE @Block@ object is a parent for each word that makes up the line. Words are represented by @Block@ objects of type WORD.
--
-- Use the MaxResults parameter to limit the number of blocks that are returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetDocumentTextDetection@ , and populate the @NextToken@ request parameter with the token value that's returned from the previous call to @GetDocumentTextDetection@ .
--
-- For more information, see <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-detecting.html Document Text Detection> .
--
module Network.AWS.Textract.GetDocumentTextDetection
    (
    -- * Creating a Request
      getDocumentTextDetection
    , GetDocumentTextDetection
    -- * Request Lenses
    , gdtdNextToken
    , gdtdMaxResults
    , gdtdJobId

    -- * Destructuring the Response
    , getDocumentTextDetectionResponse
    , GetDocumentTextDetectionResponse
    -- * Response Lenses
    , gdtdrsDocumentMetadata
    , gdtdrsBlocks
    , gdtdrsWarnings
    , gdtdrsNextToken
    , gdtdrsStatusMessage
    , gdtdrsJobStatus
    , gdtdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Textract.Types
import Network.AWS.Textract.Types.Product

-- | /See:/ 'getDocumentTextDetection' smart constructor.
data GetDocumentTextDetection = GetDocumentTextDetection'
  { _gdtdNextToken  :: !(Maybe Text)
  , _gdtdMaxResults :: !(Maybe Nat)
  , _gdtdJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentTextDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdtdNextToken' - If the previous response was incomplete (because there are more blocks to retrieve), Amazon Textract returns a pagination token in the response. You can use this pagination token to retrieve the next set of blocks.
--
-- * 'gdtdMaxResults' - The maximum number of results to return per paginated call. The largest value you can specify is 1,000. If you specify a value greater than 1,000, a maximum of 1,000 results is returned. The default value is 1,000.
--
-- * 'gdtdJobId' - A unique identifier for the text detection job. The @JobId@ is returned from @StartDocumentTextDetection@ .
getDocumentTextDetection
    :: Text -- ^ 'gdtdJobId'
    -> GetDocumentTextDetection
getDocumentTextDetection pJobId_ =
  GetDocumentTextDetection'
    {_gdtdNextToken = Nothing, _gdtdMaxResults = Nothing, _gdtdJobId = pJobId_}


-- | If the previous response was incomplete (because there are more blocks to retrieve), Amazon Textract returns a pagination token in the response. You can use this pagination token to retrieve the next set of blocks.
gdtdNextToken :: Lens' GetDocumentTextDetection (Maybe Text)
gdtdNextToken = lens _gdtdNextToken (\ s a -> s{_gdtdNextToken = a})

-- | The maximum number of results to return per paginated call. The largest value you can specify is 1,000. If you specify a value greater than 1,000, a maximum of 1,000 results is returned. The default value is 1,000.
gdtdMaxResults :: Lens' GetDocumentTextDetection (Maybe Natural)
gdtdMaxResults = lens _gdtdMaxResults (\ s a -> s{_gdtdMaxResults = a}) . mapping _Nat

-- | A unique identifier for the text detection job. The @JobId@ is returned from @StartDocumentTextDetection@ .
gdtdJobId :: Lens' GetDocumentTextDetection Text
gdtdJobId = lens _gdtdJobId (\ s a -> s{_gdtdJobId = a})

instance AWSRequest GetDocumentTextDetection where
        type Rs GetDocumentTextDetection =
             GetDocumentTextDetectionResponse
        request = postJSON textract
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentTextDetectionResponse' <$>
                   (x .?> "DocumentMetadata") <*>
                     (x .?> "Blocks" .!@ mempty)
                     <*> (x .?> "Warnings" .!@ mempty)
                     <*> (x .?> "NextToken")
                     <*> (x .?> "StatusMessage")
                     <*> (x .?> "JobStatus")
                     <*> (pure (fromEnum s)))

instance Hashable GetDocumentTextDetection where

instance NFData GetDocumentTextDetection where

instance ToHeaders GetDocumentTextDetection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Textract.GetDocumentTextDetection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDocumentTextDetection where
        toJSON GetDocumentTextDetection'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gdtdNextToken,
                  ("MaxResults" .=) <$> _gdtdMaxResults,
                  Just ("JobId" .= _gdtdJobId)])

instance ToPath GetDocumentTextDetection where
        toPath = const "/"

instance ToQuery GetDocumentTextDetection where
        toQuery = const mempty

-- | /See:/ 'getDocumentTextDetectionResponse' smart constructor.
data GetDocumentTextDetectionResponse = GetDocumentTextDetectionResponse'
  { _gdtdrsDocumentMetadata :: !(Maybe DocumentMetadata)
  , _gdtdrsBlocks           :: !(Maybe [Block])
  , _gdtdrsWarnings         :: !(Maybe [Warning])
  , _gdtdrsNextToken        :: !(Maybe Text)
  , _gdtdrsStatusMessage    :: !(Maybe Text)
  , _gdtdrsJobStatus        :: !(Maybe JobStatus)
  , _gdtdrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentTextDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdtdrsDocumentMetadata' - Information about a document that Amazon Textract processed. @DocumentMetadata@ is returned in every page of paginated responses from an Amazon Textract video operation.
--
-- * 'gdtdrsBlocks' - The results of the text-detection operation.
--
-- * 'gdtdrsWarnings' - A list of warnings that occurred during the document text-detection operation.
--
-- * 'gdtdrsNextToken' - If the response is truncated, Amazon Textract returns this token. You can use this token in the subsequent request to retrieve the next set of text-detection results.
--
-- * 'gdtdrsStatusMessage' - The current status of an asynchronous document text-detection operation.
--
-- * 'gdtdrsJobStatus' - The current status of the text detection job.
--
-- * 'gdtdrsResponseStatus' - -- | The response status code.
getDocumentTextDetectionResponse
    :: Int -- ^ 'gdtdrsResponseStatus'
    -> GetDocumentTextDetectionResponse
getDocumentTextDetectionResponse pResponseStatus_ =
  GetDocumentTextDetectionResponse'
    { _gdtdrsDocumentMetadata = Nothing
    , _gdtdrsBlocks = Nothing
    , _gdtdrsWarnings = Nothing
    , _gdtdrsNextToken = Nothing
    , _gdtdrsStatusMessage = Nothing
    , _gdtdrsJobStatus = Nothing
    , _gdtdrsResponseStatus = pResponseStatus_
    }


-- | Information about a document that Amazon Textract processed. @DocumentMetadata@ is returned in every page of paginated responses from an Amazon Textract video operation.
gdtdrsDocumentMetadata :: Lens' GetDocumentTextDetectionResponse (Maybe DocumentMetadata)
gdtdrsDocumentMetadata = lens _gdtdrsDocumentMetadata (\ s a -> s{_gdtdrsDocumentMetadata = a})

-- | The results of the text-detection operation.
gdtdrsBlocks :: Lens' GetDocumentTextDetectionResponse [Block]
gdtdrsBlocks = lens _gdtdrsBlocks (\ s a -> s{_gdtdrsBlocks = a}) . _Default . _Coerce

-- | A list of warnings that occurred during the document text-detection operation.
gdtdrsWarnings :: Lens' GetDocumentTextDetectionResponse [Warning]
gdtdrsWarnings = lens _gdtdrsWarnings (\ s a -> s{_gdtdrsWarnings = a}) . _Default . _Coerce

-- | If the response is truncated, Amazon Textract returns this token. You can use this token in the subsequent request to retrieve the next set of text-detection results.
gdtdrsNextToken :: Lens' GetDocumentTextDetectionResponse (Maybe Text)
gdtdrsNextToken = lens _gdtdrsNextToken (\ s a -> s{_gdtdrsNextToken = a})

-- | The current status of an asynchronous document text-detection operation.
gdtdrsStatusMessage :: Lens' GetDocumentTextDetectionResponse (Maybe Text)
gdtdrsStatusMessage = lens _gdtdrsStatusMessage (\ s a -> s{_gdtdrsStatusMessage = a})

-- | The current status of the text detection job.
gdtdrsJobStatus :: Lens' GetDocumentTextDetectionResponse (Maybe JobStatus)
gdtdrsJobStatus = lens _gdtdrsJobStatus (\ s a -> s{_gdtdrsJobStatus = a})

-- | -- | The response status code.
gdtdrsResponseStatus :: Lens' GetDocumentTextDetectionResponse Int
gdtdrsResponseStatus = lens _gdtdrsResponseStatus (\ s a -> s{_gdtdrsResponseStatus = a})

instance NFData GetDocumentTextDetectionResponse
         where
