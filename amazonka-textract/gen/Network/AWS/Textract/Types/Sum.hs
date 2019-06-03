{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Textract.Types.Sum where

import Network.AWS.Prelude

data BlockType
  = Line
  | Word
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BlockType where
    parser = takeLowerText >>= \case
        "line" -> pure Line
        "word" -> pure Word
        e -> fromTextError $ "Failure parsing BlockType from value: '" <> e
           <> "'. Accepted values: line, word"

instance ToText BlockType where
    toText = \case
        Line -> "LINE"
        Word -> "WORD"

instance Hashable     BlockType
instance NFData       BlockType
instance ToByteString BlockType
instance ToQuery      BlockType
instance ToHeader     BlockType

instance FromJSON BlockType where
    parseJSON = parseJSONText "BlockType"

data RelationshipType
  = Child
  | Value
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RelationshipType where
    parser = takeLowerText >>= \case
        "child" -> pure Child
        "value" -> pure Value
        e -> fromTextError $ "Failure parsing RelationshipType from value: '" <> e
           <> "'. Accepted values: child, value"

instance ToText RelationshipType where
    toText = \case
        Child -> "CHILD"
        Value -> "VALUE"

instance Hashable     RelationshipType
instance NFData       RelationshipType
instance ToByteString RelationshipType
instance ToQuery      RelationshipType
instance ToHeader     RelationshipType

instance FromJSON RelationshipType where
    parseJSON = parseJSONText "RelationshipType"
