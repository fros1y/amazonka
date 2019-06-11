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
  = Cell
  | KeyValueSet
  | Line
  | Page
  | SelectionElement
  | Table
  | Word
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BlockType where
    parser = takeLowerText >>= \case
        "cell" -> pure Cell
        "key_value_set" -> pure KeyValueSet
        "line" -> pure Line
        "page" -> pure Page
        "selection_element" -> pure SelectionElement
        "table" -> pure Table
        "word" -> pure Word
        e -> fromTextError $ "Failure parsing BlockType from value: '" <> e
           <> "'. Accepted values: cell, key_value_set, line, page, selection_element, table, word"

instance ToText BlockType where
    toText = \case
        Cell -> "CELL"
        KeyValueSet -> "KEY_VALUE_SET"
        Line -> "LINE"
        Page -> "PAGE"
        SelectionElement -> "SELECTION_ELEMENT"
        Table -> "TABLE"
        Word -> "WORD"

instance Hashable     BlockType
instance NFData       BlockType
instance ToByteString BlockType
instance ToQuery      BlockType
instance ToHeader     BlockType

instance FromJSON BlockType where
    parseJSON = parseJSONText "BlockType"

data EntityType
  = ETKey
  | ETValue
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntityType where
    parser = takeLowerText >>= \case
        "key" -> pure ETKey
        "value" -> pure ETValue
        e -> fromTextError $ "Failure parsing EntityType from value: '" <> e
           <> "'. Accepted values: key, value"

instance ToText EntityType where
    toText = \case
        ETKey -> "KEY"
        ETValue -> "VALUE"

instance Hashable     EntityType
instance NFData       EntityType
instance ToByteString EntityType
instance ToQuery      EntityType
instance ToHeader     EntityType

instance FromJSON EntityType where
    parseJSON = parseJSONText "EntityType"

data FeatureType
  = Forms
  | Tables
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FeatureType where
    parser = takeLowerText >>= \case
        "forms" -> pure Forms
        "tables" -> pure Tables
        e -> fromTextError $ "Failure parsing FeatureType from value: '" <> e
           <> "'. Accepted values: forms, tables"

instance ToText FeatureType where
    toText = \case
        Forms -> "FORMS"
        Tables -> "TABLES"

instance Hashable     FeatureType
instance NFData       FeatureType
instance ToByteString FeatureType
instance ToQuery      FeatureType
instance ToHeader     FeatureType

instance ToJSON FeatureType where
    toJSON = toJSONText

data JobStatus
  = Failed
  | InProgress
  | PartialSuccess
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "partial_success" -> pure PartialSuccess
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, partial_success, succeeded"

instance ToText JobStatus where
    toText = \case
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        PartialSuccess -> "PARTIAL_SUCCESS"
        Succeeded -> "SUCCEEDED"

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

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

data SelectionStatus
  = NotSelected
  | Selected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SelectionStatus where
    parser = takeLowerText >>= \case
        "not_selected" -> pure NotSelected
        "selected" -> pure Selected
        e -> fromTextError $ "Failure parsing SelectionStatus from value: '" <> e
           <> "'. Accepted values: not_selected, selected"

instance ToText SelectionStatus where
    toText = \case
        NotSelected -> "NOT_SELECTED"
        Selected -> "SELECTED"

instance Hashable     SelectionStatus
instance NFData       SelectionStatus
instance ToByteString SelectionStatus
instance ToQuery      SelectionStatus
instance ToHeader     SelectionStatus

instance FromJSON SelectionStatus where
    parseJSON = parseJSONText "SelectionStatus"
