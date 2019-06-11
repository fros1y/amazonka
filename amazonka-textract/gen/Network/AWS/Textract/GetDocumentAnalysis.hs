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
-- Module      : Network.AWS.Textract.GetDocumentAnalysis
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the results for an Amazon Textract asynchronous operation that analyzes text in a document.
--
--
-- You start asynchronous text analysis by calling 'StartDocumentAnalysis' , which returns a job identifier (@JobId@ ). When the text analysis operation finishes, Amazon Textract publishes a completion status to the Amazon Simple Notification Service (Amazon SNS) topic that's registered in the initial call to @StartDocumentAnalysis@ . To get the results of the text-detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetDocumentAnalysis@ , and pass the job identifier (@JobId@ ) from the initial call to @StartDocumentAnalysis@ .
--
-- @GetDocumentAnalysis@ returns an array of 'Block' objects. The following types of information are returned:
--
--     * Words and lines that are related to nearby lines and words. The related information is returned in two 'Block' objects each of type @KEY_VALUE_SET@ : a KEY Block object and a VALUE Block object. For example, /Name: Ana Silva Carolina/ contains a key and value. /Name:/ is the key. /Ana Silva Carolina/ is the value.
--
--     * Table and table cell data. A TABLE Block object contains information about a detected table. A CELL Block object is returned for each cell in a table.
--
--     * Selectable elements such as checkboxes and radio buttons. A SELECTION_ELEMENT Block object contains information about a selectable element.
--
--     * Lines and words of text. A LINE Block object contains one or more WORD Block objects.
--
--
--
-- Use the @MaxResults@ parameter to limit the number of blocks returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetDocumentAnalysis@ , and populate the @NextToken@ request parameter with the token value that's returned from the previous call to @GetDocumentAnalysis@ .
--
-- For more information, see <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-analyzing.html Document Text Analysis> .
--
module Network.AWS.Textract.GetDocumentAnalysis
    (
    -- * Creating a Request
      getDocumentAnalysis
    , GetDocumentAnalysis
    -- * Request Lenses
    , gdaNextToken
    , gdaMaxResults
    , gdaJobId

    -- * Destructuring the Response
    , getDocumentAnalysisResponse
    , GetDocumentAnalysisResponse
    -- * Response Lenses
    , gdarsDocumentMetadata
    , gdarsBlocks
    , gdarsWarnings
    , gdarsNextToken
    , gdarsStatusMessage
    , gdarsJobStatus
    , gdarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Textract.Types
import Network.AWS.Textract.Types.Product

-- | /See:/ 'getDocumentAnalysis' smart constructor.
data GetDocumentAnalysis = GetDocumentAnalysis'
  { _gdaNextToken  :: !(Maybe Text)
  , _gdaMaxResults :: !(Maybe Nat)
  , _gdaJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentAnalysis' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdaNextToken' - If the previous response was incomplete (because there are more blocks to retrieve), Amazon Textract returns a pagination token in the response. You can use this pagination token to retrieve the next set of blocks.
--
-- * 'gdaMaxResults' - The maximum number of results to return per paginated call. The largest value that you can specify is 1,000. If you specify a value greater than 1,000, a maximum of 1,000 results is returned. The default value is 1,000.
--
-- * 'gdaJobId' - A unique identifier for the text-detection job. The @JobId@ is returned from @StartDocumentAnalysis@ .
getDocumentAnalysis
    :: Text -- ^ 'gdaJobId'
    -> GetDocumentAnalysis
getDocumentAnalysis pJobId_ =
  GetDocumentAnalysis'
    {_gdaNextToken = Nothing, _gdaMaxResults = Nothing, _gdaJobId = pJobId_}


-- | If the previous response was incomplete (because there are more blocks to retrieve), Amazon Textract returns a pagination token in the response. You can use this pagination token to retrieve the next set of blocks.
gdaNextToken :: Lens' GetDocumentAnalysis (Maybe Text)
gdaNextToken = lens _gdaNextToken (\ s a -> s{_gdaNextToken = a})

-- | The maximum number of results to return per paginated call. The largest value that you can specify is 1,000. If you specify a value greater than 1,000, a maximum of 1,000 results is returned. The default value is 1,000.
gdaMaxResults :: Lens' GetDocumentAnalysis (Maybe Natural)
gdaMaxResults = lens _gdaMaxResults (\ s a -> s{_gdaMaxResults = a}) . mapping _Nat

-- | A unique identifier for the text-detection job. The @JobId@ is returned from @StartDocumentAnalysis@ .
gdaJobId :: Lens' GetDocumentAnalysis Text
gdaJobId = lens _gdaJobId (\ s a -> s{_gdaJobId = a})

instance AWSRequest GetDocumentAnalysis where
        type Rs GetDocumentAnalysis =
             GetDocumentAnalysisResponse
        request = postJSON textract
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentAnalysisResponse' <$>
                   (x .?> "DocumentMetadata") <*>
                     (x .?> "Blocks" .!@ mempty)
                     <*> (x .?> "Warnings" .!@ mempty)
                     <*> (x .?> "NextToken")
                     <*> (x .?> "StatusMessage")
                     <*> (x .?> "JobStatus")
                     <*> (pure (fromEnum s)))

instance Hashable GetDocumentAnalysis where

instance NFData GetDocumentAnalysis where

instance ToHeaders GetDocumentAnalysis where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Textract.GetDocumentAnalysis" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDocumentAnalysis where
        toJSON GetDocumentAnalysis'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gdaNextToken,
                  ("MaxResults" .=) <$> _gdaMaxResults,
                  Just ("JobId" .= _gdaJobId)])

instance ToPath GetDocumentAnalysis where
        toPath = const "/"

instance ToQuery GetDocumentAnalysis where
        toQuery = const mempty

-- | /See:/ 'getDocumentAnalysisResponse' smart constructor.
data GetDocumentAnalysisResponse = GetDocumentAnalysisResponse'
  { _gdarsDocumentMetadata :: !(Maybe DocumentMetadata)
  , _gdarsBlocks           :: !(Maybe [Block])
  , _gdarsWarnings         :: !(Maybe [Warning])
  , _gdarsNextToken        :: !(Maybe Text)
  , _gdarsStatusMessage    :: !(Maybe Text)
  , _gdarsJobStatus        :: !(Maybe JobStatus)
  , _gdarsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentAnalysisResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdarsDocumentMetadata' - Information about a document that Amazon Textract processed. @DocumentMetadata@ is returned in every page of paginated responses from an Amazon Textract video operation.
--
-- * 'gdarsBlocks' - The results of the text analysis operation.
--
-- * 'gdarsWarnings' - A list of warnings that occurred during the document analysis operation.
--
-- * 'gdarsNextToken' - If the response is truncated, Amazon Textract returns this token. You can use this token in the subsequent request to retrieve the next set of text detection results.
--
-- * 'gdarsStatusMessage' - The current status of an asynchronous document analysis operation.
--
-- * 'gdarsJobStatus' - The current status of the text detection job.
--
-- * 'gdarsResponseStatus' - -- | The response status code.
getDocumentAnalysisResponse
    :: Int -- ^ 'gdarsResponseStatus'
    -> GetDocumentAnalysisResponse
getDocumentAnalysisResponse pResponseStatus_ =
  GetDocumentAnalysisResponse'
    { _gdarsDocumentMetadata = Nothing
    , _gdarsBlocks = Nothing
    , _gdarsWarnings = Nothing
    , _gdarsNextToken = Nothing
    , _gdarsStatusMessage = Nothing
    , _gdarsJobStatus = Nothing
    , _gdarsResponseStatus = pResponseStatus_
    }


-- | Information about a document that Amazon Textract processed. @DocumentMetadata@ is returned in every page of paginated responses from an Amazon Textract video operation.
gdarsDocumentMetadata :: Lens' GetDocumentAnalysisResponse (Maybe DocumentMetadata)
gdarsDocumentMetadata = lens _gdarsDocumentMetadata (\ s a -> s{_gdarsDocumentMetadata = a})

-- | The results of the text analysis operation.
gdarsBlocks :: Lens' GetDocumentAnalysisResponse [Block]
gdarsBlocks = lens _gdarsBlocks (\ s a -> s{_gdarsBlocks = a}) . _Default . _Coerce

-- | A list of warnings that occurred during the document analysis operation.
gdarsWarnings :: Lens' GetDocumentAnalysisResponse [Warning]
gdarsWarnings = lens _gdarsWarnings (\ s a -> s{_gdarsWarnings = a}) . _Default . _Coerce

-- | If the response is truncated, Amazon Textract returns this token. You can use this token in the subsequent request to retrieve the next set of text detection results.
gdarsNextToken :: Lens' GetDocumentAnalysisResponse (Maybe Text)
gdarsNextToken = lens _gdarsNextToken (\ s a -> s{_gdarsNextToken = a})

-- | The current status of an asynchronous document analysis operation.
gdarsStatusMessage :: Lens' GetDocumentAnalysisResponse (Maybe Text)
gdarsStatusMessage = lens _gdarsStatusMessage (\ s a -> s{_gdarsStatusMessage = a})

-- | The current status of the text detection job.
gdarsJobStatus :: Lens' GetDocumentAnalysisResponse (Maybe JobStatus)
gdarsJobStatus = lens _gdarsJobStatus (\ s a -> s{_gdarsJobStatus = a})

-- | -- | The response status code.
gdarsResponseStatus :: Lens' GetDocumentAnalysisResponse Int
gdarsResponseStatus = lens _gdarsResponseStatus (\ s a -> s{_gdarsResponseStatus = a})

instance NFData GetDocumentAnalysisResponse where
