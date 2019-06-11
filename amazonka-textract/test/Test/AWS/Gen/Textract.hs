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
--         , requestStartDocumentAnalysis $
--             startDocumentAnalysis
--
--         , requestAnalyzeDocument $
--             analyzeDocument
--
--         , requestGetDocumentTextDetection $
--             getDocumentTextDetection
--
--         , requestStartDocumentTextDetection $
--             startDocumentTextDetection
--
--         , requestGetDocumentAnalysis $
--             getDocumentAnalysis
--
--           ]

--     , testGroup "response"
--         [ responseDetectDocumentText $
--             detectDocumentTextResponse
--
--         , responseStartDocumentAnalysis $
--             startDocumentAnalysisResponse
--
--         , responseAnalyzeDocument $
--             analyzeDocumentResponse
--
--         , responseGetDocumentTextDetection $
--             getDocumentTextDetectionResponse
--
--         , responseStartDocumentTextDetection $
--             startDocumentTextDetectionResponse
--
--         , responseGetDocumentAnalysis $
--             getDocumentAnalysisResponse
--
--           ]
--     ]

-- Requests

requestDetectDocumentText :: DetectDocumentText -> TestTree
requestDetectDocumentText = req
    "DetectDocumentText"
    "fixture/DetectDocumentText.yaml"

requestStartDocumentAnalysis :: StartDocumentAnalysis -> TestTree
requestStartDocumentAnalysis = req
    "StartDocumentAnalysis"
    "fixture/StartDocumentAnalysis.yaml"

requestAnalyzeDocument :: AnalyzeDocument -> TestTree
requestAnalyzeDocument = req
    "AnalyzeDocument"
    "fixture/AnalyzeDocument.yaml"

requestGetDocumentTextDetection :: GetDocumentTextDetection -> TestTree
requestGetDocumentTextDetection = req
    "GetDocumentTextDetection"
    "fixture/GetDocumentTextDetection.yaml"

requestStartDocumentTextDetection :: StartDocumentTextDetection -> TestTree
requestStartDocumentTextDetection = req
    "StartDocumentTextDetection"
    "fixture/StartDocumentTextDetection.yaml"

requestGetDocumentAnalysis :: GetDocumentAnalysis -> TestTree
requestGetDocumentAnalysis = req
    "GetDocumentAnalysis"
    "fixture/GetDocumentAnalysis.yaml"

-- Responses

responseDetectDocumentText :: DetectDocumentTextResponse -> TestTree
responseDetectDocumentText = res
    "DetectDocumentTextResponse"
    "fixture/DetectDocumentTextResponse.proto"
    textract
    (Proxy :: Proxy DetectDocumentText)

responseStartDocumentAnalysis :: StartDocumentAnalysisResponse -> TestTree
responseStartDocumentAnalysis = res
    "StartDocumentAnalysisResponse"
    "fixture/StartDocumentAnalysisResponse.proto"
    textract
    (Proxy :: Proxy StartDocumentAnalysis)

responseAnalyzeDocument :: AnalyzeDocumentResponse -> TestTree
responseAnalyzeDocument = res
    "AnalyzeDocumentResponse"
    "fixture/AnalyzeDocumentResponse.proto"
    textract
    (Proxy :: Proxy AnalyzeDocument)

responseGetDocumentTextDetection :: GetDocumentTextDetectionResponse -> TestTree
responseGetDocumentTextDetection = res
    "GetDocumentTextDetectionResponse"
    "fixture/GetDocumentTextDetectionResponse.proto"
    textract
    (Proxy :: Proxy GetDocumentTextDetection)

responseStartDocumentTextDetection :: StartDocumentTextDetectionResponse -> TestTree
responseStartDocumentTextDetection = res
    "StartDocumentTextDetectionResponse"
    "fixture/StartDocumentTextDetectionResponse.proto"
    textract
    (Proxy :: Proxy StartDocumentTextDetection)

responseGetDocumentAnalysis :: GetDocumentAnalysisResponse -> TestTree
responseGetDocumentAnalysis = res
    "GetDocumentAnalysisResponse"
    "fixture/GetDocumentAnalysisResponse.proto"
    textract
    (Proxy :: Proxy GetDocumentAnalysis)
