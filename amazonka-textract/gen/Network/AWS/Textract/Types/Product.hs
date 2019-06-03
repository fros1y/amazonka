{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Textract.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Textract.Types.Sum

-- | /See:/ 'block' smart constructor.
data Block = Block'
  { _bText          :: !(Maybe Text)
  , _bConfidence    :: !(Maybe Double)
  , _bRelationships :: !(Maybe [Relationship])
  , _bGeometry      :: !(Maybe Geometry)
  , _bId            :: !(Maybe Text)
  , _bBlockType     :: !(Maybe BlockType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Block' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bText' - The word or line of text that's recognized by Amazon Textract.
--
-- * 'bConfidence' - The confidence score that Amazon Textract has in the accuracy of the recognized text and the accuracy of the geometry points around the recognized text.
--
-- * 'bRelationships' - A list of child blocks of the current block. For example, a LINE object has child blocks for each WORD block that's part of the line of text. There aren't Relationship objects in the list for relationships that don't exist, such as when the current block has no child blocks.
--
-- * 'bGeometry' - The location of the recognized text on the image. It includes an axis-aligned, coarse bounding box that surrounds the text, and a finer-grain polygon for more accurate spatial information.
--
-- * 'bId' - The identifier for the recognized text. The identifier is only unique for a single operation.
--
-- * 'bBlockType' - The type of text item that's recognized. In operations for text detection, the following types are returned:    * PAGE - Contains a list of the LINE Block objects that are detected on a document page.    * WORD - A word detected on a document page. A word is one or more ISO basic Latin script characters that aren't separated by spaces.    * LINE - A string of tab-delimited, contiguous words that are detected on a document page.
block
    :: Block
block =
  Block'
    { _bText = Nothing
    , _bConfidence = Nothing
    , _bRelationships = Nothing
    , _bGeometry = Nothing
    , _bId = Nothing
    , _bBlockType = Nothing
    }


-- | The word or line of text that's recognized by Amazon Textract.
bText :: Lens' Block (Maybe Text)
bText = lens _bText (\ s a -> s{_bText = a})

-- | The confidence score that Amazon Textract has in the accuracy of the recognized text and the accuracy of the geometry points around the recognized text.
bConfidence :: Lens' Block (Maybe Double)
bConfidence = lens _bConfidence (\ s a -> s{_bConfidence = a})

-- | A list of child blocks of the current block. For example, a LINE object has child blocks for each WORD block that's part of the line of text. There aren't Relationship objects in the list for relationships that don't exist, such as when the current block has no child blocks.
bRelationships :: Lens' Block [Relationship]
bRelationships = lens _bRelationships (\ s a -> s{_bRelationships = a}) . _Default . _Coerce

-- | The location of the recognized text on the image. It includes an axis-aligned, coarse bounding box that surrounds the text, and a finer-grain polygon for more accurate spatial information.
bGeometry :: Lens' Block (Maybe Geometry)
bGeometry = lens _bGeometry (\ s a -> s{_bGeometry = a})

-- | The identifier for the recognized text. The identifier is only unique for a single operation.
bId :: Lens' Block (Maybe Text)
bId = lens _bId (\ s a -> s{_bId = a})

-- | The type of text item that's recognized. In operations for text detection, the following types are returned:    * PAGE - Contains a list of the LINE Block objects that are detected on a document page.    * WORD - A word detected on a document page. A word is one or more ISO basic Latin script characters that aren't separated by spaces.    * LINE - A string of tab-delimited, contiguous words that are detected on a document page.
bBlockType :: Lens' Block (Maybe BlockType)
bBlockType = lens _bBlockType (\ s a -> s{_bBlockType = a})

instance FromJSON Block where
        parseJSON
          = withObject "Block"
              (\ x ->
                 Block' <$>
                   (x .:? "Text") <*> (x .:? "Confidence") <*>
                     (x .:? "Relationships" .!= mempty)
                     <*> (x .:? "Geometry")
                     <*> (x .:? "Id")
                     <*> (x .:? "BlockType"))

instance Hashable Block where

instance NFData Block where

-- | Identifies the bounding box around the object, face or text. The @left@ (x-coordinate) and @top@ (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0).
--
--
-- The @top@ and @left@ values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a @left@ value of 0.5 (350/700) and a @top@ value of 0.25 (50/200).
--
-- The @width@ and @height@ values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1.
--
--
-- /See:/ 'boundingBox' smart constructor.
data BoundingBox = BoundingBox'
  { _bbHeight :: !(Maybe Double)
  , _bbLeft   :: !(Maybe Double)
  , _bbWidth  :: !(Maybe Double)
  , _bbTop    :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BoundingBox' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bbHeight' - Height of the bounding box as a ratio of the overall image height.
--
-- * 'bbLeft' - Left coordinate of the bounding box as a ratio of overall image width.
--
-- * 'bbWidth' - Width of the bounding box as a ratio of the overall image width.
--
-- * 'bbTop' - Top coordinate of the bounding box as a ratio of overall image height.
boundingBox
    :: BoundingBox
boundingBox =
  BoundingBox'
    { _bbHeight = Nothing
    , _bbLeft = Nothing
    , _bbWidth = Nothing
    , _bbTop = Nothing
    }


-- | Height of the bounding box as a ratio of the overall image height.
bbHeight :: Lens' BoundingBox (Maybe Double)
bbHeight = lens _bbHeight (\ s a -> s{_bbHeight = a})

-- | Left coordinate of the bounding box as a ratio of overall image width.
bbLeft :: Lens' BoundingBox (Maybe Double)
bbLeft = lens _bbLeft (\ s a -> s{_bbLeft = a})

-- | Width of the bounding box as a ratio of the overall image width.
bbWidth :: Lens' BoundingBox (Maybe Double)
bbWidth = lens _bbWidth (\ s a -> s{_bbWidth = a})

-- | Top coordinate of the bounding box as a ratio of overall image height.
bbTop :: Lens' BoundingBox (Maybe Double)
bbTop = lens _bbTop (\ s a -> s{_bbTop = a})

instance FromJSON BoundingBox where
        parseJSON
          = withObject "BoundingBox"
              (\ x ->
                 BoundingBox' <$>
                   (x .:? "Height") <*> (x .:? "Left") <*>
                     (x .:? "Width")
                     <*> (x .:? "Top"))

instance Hashable BoundingBox where

instance NFData BoundingBox where

-- | <The>If you're using an AWS SDK to call Amazon Textract, you might not need to base64-encode image bytes that are passed using the Bytes field.
--
--
--
--
--
-- /See:/ 'document' smart constructor.
data Document = Document'
  { _dS3Object :: !(Maybe S3Object)
  , _dBytes    :: !(Maybe Base64)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Document' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dS3Object' - Identifies an S3 object as the document source.
--
-- * 'dBytes' - Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
document
    :: Document
document = Document' {_dS3Object = Nothing, _dBytes = Nothing}


-- | Identifies an S3 object as the document source.
dS3Object :: Lens' Document (Maybe S3Object)
dS3Object = lens _dS3Object (\ s a -> s{_dS3Object = a})

-- | Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
dBytes :: Lens' Document (Maybe ByteString)
dBytes = lens _dBytes (\ s a -> s{_dBytes = a}) . mapping _Base64

instance Hashable Document where

instance NFData Document where

instance ToJSON Document where
        toJSON Document'{..}
          = object
              (catMaybes
                 [("S3Object" .=) <$> _dS3Object,
                  ("Bytes" .=) <$> _dBytes])

-- | /See:/ 'documentMetadata' smart constructor.
newtype DocumentMetadata = DocumentMetadata'
  { _dmPages :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmPages' - The number of pages that are detected in the document.
documentMetadata
    :: DocumentMetadata
documentMetadata = DocumentMetadata' {_dmPages = Nothing}


-- | The number of pages that are detected in the document.
dmPages :: Lens' DocumentMetadata (Maybe Natural)
dmPages = lens _dmPages (\ s a -> s{_dmPages = a}) . mapping _Nat

instance FromJSON DocumentMetadata where
        parseJSON
          = withObject "DocumentMetadata"
              (\ x -> DocumentMetadata' <$> (x .:? "Pages"))

instance Hashable DocumentMetadata where

instance NFData DocumentMetadata where

-- | Information about where text detected by is located on a page.
--
--
--
-- /See:/ 'geometry' smart constructor.
data Geometry = Geometry'
  { _gBoundingBox :: !(Maybe BoundingBox)
  , _gPolygon     :: !(Maybe [Point])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Geometry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gBoundingBox' - An axis-aligned coarse representation of the detected text's location on the page.
--
-- * 'gPolygon' - Within the bounding box, a fine-grained polygon around the detected page.
geometry
    :: Geometry
geometry = Geometry' {_gBoundingBox = Nothing, _gPolygon = Nothing}


-- | An axis-aligned coarse representation of the detected text's location on the page.
gBoundingBox :: Lens' Geometry (Maybe BoundingBox)
gBoundingBox = lens _gBoundingBox (\ s a -> s{_gBoundingBox = a})

-- | Within the bounding box, a fine-grained polygon around the detected page.
gPolygon :: Lens' Geometry [Point]
gPolygon = lens _gPolygon (\ s a -> s{_gPolygon = a}) . _Default . _Coerce

instance FromJSON Geometry where
        parseJSON
          = withObject "Geometry"
              (\ x ->
                 Geometry' <$>
                   (x .:? "BoundingBox") <*>
                     (x .:? "Polygon" .!= mempty))

instance Hashable Geometry where

instance NFData Geometry where

-- | The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.
--
--
-- An array of @Point@ objects, @Polygon@ , is returned by . @Polygon@ represents a fine-grained polygon around detected text. For more information, see .
--
--
-- /See:/ 'point' smart constructor.
data Point = Point'
  { _pX :: !(Maybe Double)
  , _pY :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Point' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pX' - The value of the X coordinate for a point on a @Polygon@ .
--
-- * 'pY' - The value of the Y coordinate for a point on a @Polygon@ .
point
    :: Point
point = Point' {_pX = Nothing, _pY = Nothing}


-- | The value of the X coordinate for a point on a @Polygon@ .
pX :: Lens' Point (Maybe Double)
pX = lens _pX (\ s a -> s{_pX = a})

-- | The value of the Y coordinate for a point on a @Polygon@ .
pY :: Lens' Point (Maybe Double)
pY = lens _pY (\ s a -> s{_pY = a})

instance FromJSON Point where
        parseJSON
          = withObject "Point"
              (\ x -> Point' <$> (x .:? "X") <*> (x .:? "Y"))

instance Hashable Point where

instance NFData Point where

-- | /See:/ 'relationship' smart constructor.
data Relationship = Relationship'
  { _rIds  :: !(Maybe [Text])
  , _rType :: !(Maybe RelationshipType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Relationship' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rIds' - An array of IDs for related blocks. You can get the type of the relationship from the Type element.
--
-- * 'rType' - The type of relationship that the blocks in the IDs array have with the current block. The relationship can be VALUE or CHILD.
relationship
    :: Relationship
relationship = Relationship' {_rIds = Nothing, _rType = Nothing}


-- | An array of IDs for related blocks. You can get the type of the relationship from the Type element.
rIds :: Lens' Relationship [Text]
rIds = lens _rIds (\ s a -> s{_rIds = a}) . _Default . _Coerce

-- | The type of relationship that the blocks in the IDs array have with the current block. The relationship can be VALUE or CHILD.
rType :: Lens' Relationship (Maybe RelationshipType)
rType = lens _rType (\ s a -> s{_rType = a})

instance FromJSON Relationship where
        parseJSON
          = withObject "Relationship"
              (\ x ->
                 Relationship' <$>
                   (x .:? "Ids" .!= mempty) <*> (x .:? "Type"))

instance Hashable Relationship where

instance NFData Relationship where

-- | Provides the S3 bucket name and object name.
--
--
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
--
-- For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see 'manage-access-resource-policies' .
--
--
-- /See:/ 's3Object' smart constructor.
data S3Object = S3Object'
  { _soBucket  :: !(Maybe Text)
  , _soName    :: !(Maybe Text)
  , _soVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Object' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soBucket' - Name of the S3 bucket.
--
-- * 'soName' - S3 object key name.
--
-- * 'soVersion' - If the bucket is versioning enabled, you can specify the object version.
s3Object
    :: S3Object
s3Object =
  S3Object' {_soBucket = Nothing, _soName = Nothing, _soVersion = Nothing}


-- | Name of the S3 bucket.
soBucket :: Lens' S3Object (Maybe Text)
soBucket = lens _soBucket (\ s a -> s{_soBucket = a})

-- | S3 object key name.
soName :: Lens' S3Object (Maybe Text)
soName = lens _soName (\ s a -> s{_soName = a})

-- | If the bucket is versioning enabled, you can specify the object version.
soVersion :: Lens' S3Object (Maybe Text)
soVersion = lens _soVersion (\ s a -> s{_soVersion = a})

instance Hashable S3Object where

instance NFData S3Object where

instance ToJSON S3Object where
        toJSON S3Object'{..}
          = object
              (catMaybes
                 [("Bucket" .=) <$> _soBucket,
                  ("Name" .=) <$> _soName,
                  ("Version" .=) <$> _soVersion])
