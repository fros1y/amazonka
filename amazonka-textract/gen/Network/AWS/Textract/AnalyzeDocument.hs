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
-- Module      : Network.AWS.Textract.AnalyzeDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Analyzes an input document for relationships between detected items.
--
--
-- The types of information returned are as follows:
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
-- You can choose which type of analysis to perform by specifying the @FeatureTypes@ list.
--
-- The output is returned in a list of @BLOCK@ objects.
--
-- @AnalyzeDocument@ is a synchronous operation. To analyze documents asynchronously, use 'StartDocumentAnalysis' .
--
-- For more information, see <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-analyzing.html Document Text Analysis> .
--
module Network.AWS.Textract.AnalyzeDocument
    (
    -- * Creating a Request
      analyzeDocument
    , AnalyzeDocument
    -- * Request Lenses
    , adDocument
    , adFeatureTypes

    -- * Destructuring the Response
    , analyzeDocumentResponse
    , AnalyzeDocumentResponse
    -- * Response Lenses
    , adrsDocumentMetadata
    , adrsBlocks
    , adrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Textract.Types
import Network.AWS.Textract.Types.Product

-- | /See:/ 'analyzeDocument' smart constructor.
data AnalyzeDocument = AnalyzeDocument'
  { _adDocument     :: !Document
  , _adFeatureTypes :: ![FeatureType]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyzeDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adDocument' - The input document as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Textract operations, you can't pass image bytes. The document must be an image in JPG or PNG format. If you are using an AWS SDK to call Amazon Textract, you might not need to base64-encode image bytes passed using the @Bytes@ field.
--
-- * 'adFeatureTypes' - A list of the types of analysis to perform. Add TABLES to the list to return information about the tables detected in the input document. Add FORMS to return detected fields and the associated text. To perform both types of analysis, add TABLES and FORMS to @FeatureTypes@ .
analyzeDocument
    :: Document -- ^ 'adDocument'
    -> AnalyzeDocument
analyzeDocument pDocument_ =
  AnalyzeDocument' {_adDocument = pDocument_, _adFeatureTypes = mempty}


-- | The input document as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Textract operations, you can't pass image bytes. The document must be an image in JPG or PNG format. If you are using an AWS SDK to call Amazon Textract, you might not need to base64-encode image bytes passed using the @Bytes@ field.
adDocument :: Lens' AnalyzeDocument Document
adDocument = lens _adDocument (\ s a -> s{_adDocument = a})

-- | A list of the types of analysis to perform. Add TABLES to the list to return information about the tables detected in the input document. Add FORMS to return detected fields and the associated text. To perform both types of analysis, add TABLES and FORMS to @FeatureTypes@ .
adFeatureTypes :: Lens' AnalyzeDocument [FeatureType]
adFeatureTypes = lens _adFeatureTypes (\ s a -> s{_adFeatureTypes = a}) . _Coerce

instance AWSRequest AnalyzeDocument where
        type Rs AnalyzeDocument = AnalyzeDocumentResponse
        request = postJSON textract
        response
          = receiveJSON
              (\ s h x ->
                 AnalyzeDocumentResponse' <$>
                   (x .?> "DocumentMetadata") <*>
                     (x .?> "Blocks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable AnalyzeDocument where

instance NFData AnalyzeDocument where

instance ToHeaders AnalyzeDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Textract.AnalyzeDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AnalyzeDocument where
        toJSON AnalyzeDocument'{..}
          = object
              (catMaybes
                 [Just ("Document" .= _adDocument),
                  Just ("FeatureTypes" .= _adFeatureTypes)])

instance ToPath AnalyzeDocument where
        toPath = const "/"

instance ToQuery AnalyzeDocument where
        toQuery = const mempty

-- | /See:/ 'analyzeDocumentResponse' smart constructor.
data AnalyzeDocumentResponse = AnalyzeDocumentResponse'
  { _adrsDocumentMetadata :: !(Maybe DocumentMetadata)
  , _adrsBlocks           :: !(Maybe [Block])
  , _adrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyzeDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrsDocumentMetadata' - Metadata about the analyzed document. An example is the number of pages.
--
-- * 'adrsBlocks' - The text that's detected and analyzed by @AnalyzeDocument@ .
--
-- * 'adrsResponseStatus' - -- | The response status code.
analyzeDocumentResponse
    :: Int -- ^ 'adrsResponseStatus'
    -> AnalyzeDocumentResponse
analyzeDocumentResponse pResponseStatus_ =
  AnalyzeDocumentResponse'
    { _adrsDocumentMetadata = Nothing
    , _adrsBlocks = Nothing
    , _adrsResponseStatus = pResponseStatus_
    }


-- | Metadata about the analyzed document. An example is the number of pages.
adrsDocumentMetadata :: Lens' AnalyzeDocumentResponse (Maybe DocumentMetadata)
adrsDocumentMetadata = lens _adrsDocumentMetadata (\ s a -> s{_adrsDocumentMetadata = a})

-- | The text that's detected and analyzed by @AnalyzeDocument@ .
adrsBlocks :: Lens' AnalyzeDocumentResponse [Block]
adrsBlocks = lens _adrsBlocks (\ s a -> s{_adrsBlocks = a}) . _Default . _Coerce

-- | -- | The response status code.
adrsResponseStatus :: Lens' AnalyzeDocumentResponse Int
adrsResponseStatus = lens _adrsResponseStatus (\ s a -> s{_adrsResponseStatus = a})

instance NFData AnalyzeDocumentResponse where
