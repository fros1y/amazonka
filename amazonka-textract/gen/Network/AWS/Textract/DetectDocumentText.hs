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
-- Module      : Network.AWS.Textract.DetectDocumentText
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects text in the input document. Amazon Textract can detect lines of text and the words that make up a line of text. The input document must be an image in JPEG or PNG format. DetectDocumentText returns the detected text in an array of Block objects.
--
--
-- Each document page has as an associated Block of type PAGE. Each PAGE Block object is the parent of LINE Block objects that represent the lines of detected text on a page. A LINE Block object is a parent for each word that makes up the line. Words are represented by Block objects of type WORD.
--
module Network.AWS.Textract.DetectDocumentText
    (
    -- * Creating a Request
      detectDocumentText
    , DetectDocumentText
    -- * Request Lenses
    , ddtDocument

    -- * Destructuring the Response
    , detectDocumentTextResponse
    , DetectDocumentTextResponse
    -- * Response Lenses
    , ddtrsDocumentMetadata
    , ddtrsBlocks
    , ddtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Textract.Types
import Network.AWS.Textract.Types.Product

-- | /See:/ 'detectDocumentText' smart constructor.
newtype DetectDocumentText = DetectDocumentText'
  { _ddtDocument :: Document
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectDocumentText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddtDocument' - The input document as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes.
detectDocumentText
    :: Document -- ^ 'ddtDocument'
    -> DetectDocumentText
detectDocumentText pDocument_ = DetectDocumentText' {_ddtDocument = pDocument_}


-- | The input document as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes.
ddtDocument :: Lens' DetectDocumentText Document
ddtDocument = lens _ddtDocument (\ s a -> s{_ddtDocument = a})

instance AWSRequest DetectDocumentText where
        type Rs DetectDocumentText =
             DetectDocumentTextResponse
        request = postJSON textract
        response
          = receiveJSON
              (\ s h x ->
                 DetectDocumentTextResponse' <$>
                   (x .?> "DocumentMetadata") <*>
                     (x .?> "Blocks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DetectDocumentText where

instance NFData DetectDocumentText where

instance ToHeaders DetectDocumentText where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TextractService.DetectDocumentText" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DetectDocumentText where
        toJSON DetectDocumentText'{..}
          = object
              (catMaybes [Just ("Document" .= _ddtDocument)])

instance ToPath DetectDocumentText where
        toPath = const "/"

instance ToQuery DetectDocumentText where
        toQuery = const mempty

-- | /See:/ 'detectDocumentTextResponse' smart constructor.
data DetectDocumentTextResponse = DetectDocumentTextResponse'
  { _ddtrsDocumentMetadata :: !(Maybe DocumentMetadata)
  , _ddtrsBlocks           :: !(Maybe [Block])
  , _ddtrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectDocumentTextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddtrsDocumentMetadata' - Metadata about the document. It contains the number of pages that are detected in the document.
--
-- * 'ddtrsBlocks' - An array of Block objects that contain the text that's detected in the document.
--
-- * 'ddtrsResponseStatus' - -- | The response status code.
detectDocumentTextResponse
    :: Int -- ^ 'ddtrsResponseStatus'
    -> DetectDocumentTextResponse
detectDocumentTextResponse pResponseStatus_ =
  DetectDocumentTextResponse'
    { _ddtrsDocumentMetadata = Nothing
    , _ddtrsBlocks = Nothing
    , _ddtrsResponseStatus = pResponseStatus_
    }


-- | Metadata about the document. It contains the number of pages that are detected in the document.
ddtrsDocumentMetadata :: Lens' DetectDocumentTextResponse (Maybe DocumentMetadata)
ddtrsDocumentMetadata = lens _ddtrsDocumentMetadata (\ s a -> s{_ddtrsDocumentMetadata = a})

-- | An array of Block objects that contain the text that's detected in the document.
ddtrsBlocks :: Lens' DetectDocumentTextResponse [Block]
ddtrsBlocks = lens _ddtrsBlocks (\ s a -> s{_ddtrsBlocks = a}) . _Default . _Coerce

-- | -- | The response status code.
ddtrsResponseStatus :: Lens' DetectDocumentTextResponse Int
ddtrsResponseStatus = lens _ddtrsResponseStatus (\ s a -> s{_ddtrsResponseStatus = a})

instance NFData DetectDocumentTextResponse where
