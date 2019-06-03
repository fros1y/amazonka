{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Textract
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Textract where

import Data.Proxy
import Network.AWS.Textract
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Textract.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDetectDocumentText $
--             detectDocumentText
--
--           ]

--     , testGroup "response"
--         [ responseDetectDocumentText $
--             detectDocumentTextResponse
--
--           ]
--     ]

-- Requests

requestDetectDocumentText :: DetectDocumentText -> TestTree
requestDetectDocumentText = req
    "DetectDocumentText"
    "fixture/DetectDocumentText.yaml"

-- Responses

responseDetectDocumentText :: DetectDocumentTextResponse -> TestTree
responseDetectDocumentText = res
    "DetectDocumentTextResponse"
    "fixture/DetectDocumentTextResponse.proto"
    textract
    (Proxy :: Proxy DetectDocumentText)
