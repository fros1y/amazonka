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

-- | A @Block@ represents items that are recognized in a document within a group of pixels close to each other. The information returned in a @Block@ depends on the type of operation. In document-text detection (for example 'DetectDocumentText' ), you get information about the detected words and lines of text. In text analysis (for example 'AnalyzeDocument' ), you can also get information about the fields, tables and selection elements that are detected in the document.
--
--
-- An array of @Block@ objects is returned by both synchronous and asynchronous operations. In synchronous operations, such as 'DetectDocumentText' , the array of @Block@ objects is the entire set of results. In asynchronous operations, such as 'GetDocumentAnalysis' , the array is returned over one or more responses.
--
-- For more information, see <https://docs.aws.amazon.com/textract/latest/dg/how-it-works.html How Amazon Textract Works> .
--
--
-- /See:/ 'block' smart constructor.
data Block = Block'
  { _bColumnSpan      :: !(Maybe Nat)
  , _bText            :: !(Maybe Text)
  , _bEntityTypes     :: !(Maybe [EntityType])
  , _bColumnIndex     :: !(Maybe Nat)
  , _bPage            :: !(Maybe Nat)
  , _bRowSpan         :: !(Maybe Nat)
  , _bSelectionStatus :: !(Maybe SelectionStatus)
  , _bRowIndex        :: !(Maybe Nat)
  , _bConfidence      :: !(Maybe Double)
  , _bRelationships   :: !(Maybe [Relationship])
  , _bGeometry        :: !(Maybe Geometry)
  , _bId              :: !(Maybe Text)
  , _bBlockType       :: !(Maybe BlockType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Block' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bColumnSpan' - The number of columns that a table cell spans. @ColumnSpan@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
--
-- * 'bText' - The word or line of text that's recognized by Amazon Textract.
--
-- * 'bEntityTypes' - The type of entity. The following can be returned:     * /KEY/ - An identifier for a field on the document.     * /VALUE/ - The field text. @EntityTypes@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
--
-- * 'bColumnIndex' - The column in which a table cell appears. The first column position is 1. @ColumnIndex@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
--
-- * 'bPage' - The page in which a block was detected. @Page@ is returned by asynchronous operations. Page values greater than 1 are only returned for multi-page documents that are in PDF format. A scanned image (JPG/PNG), even if it contains multiple document pages, is always considered to be a single-page document and the value of @Page@ is always 1. Synchronous operations don't return @Page@ as every input document is considered to be a single-page document.
--
-- * 'bRowSpan' - The number of rows that a table spans. @RowSpan@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
--
-- * 'bSelectionStatus' - The selection status of a selectable element such as a radio button or checkbox.
--
-- * 'bRowIndex' - The row in which a table cell is located. The first row position is 1. @RowIndex@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
--
-- * 'bConfidence' - The confidence that Amazon Textract has in the accuracy of the recognized text and the accuracy of the geometry points around the recognized text.
--
-- * 'bRelationships' - A list of child blocks of the current block. For example a LINE object has child blocks for each WORD block that's part of the line of text. There aren't Relationship objects in the list for relationships that don't exist, such as when the current block has no child blocks. The list size can be the following:     * 0 - The block has no child blocks.     * 1 - The block has child blocks.
--
-- * 'bGeometry' - The location of the recognized text on the image. It includes an axis-aligned, coarse bounding box that surrounds the text, and a finer-grain polygon for more accurate spatial information.
--
-- * 'bId' - The identifier for the recognized text. The identifier is only unique for a single operation.
--
-- * 'bBlockType' - The type of text that's recognized in a block. In text-detection operations, the following types are returned:     * /PAGE/ - Contains a list of the LINE Block objects that are detected on a document page.     * /WORD/ - A word detected on a document page. A word is one or more ISO basic Latin script characters that aren't separated by spaces.     * /LINE/ - A string of tab-delimited, contiguous words that's detected on a document page. In text analysis operations, the following types are returned:     * /PAGE/ - Contains a list of child Block objects that are detected on a document page.     * /KEY_VALUE_SET/ - Stores the KEY and VALUE Block objects for a field that's detected on a document page. Use the @EntityType@ field to determine if a KEY_VALUE_SET object is a KEY Block object or a VALUE Block object.      * /WORD/ - A word detected on a document page. A word is one or more ISO basic Latin script characters that aren't separated by spaces that's detected on a document page.     * /LINE/ - A string of tab-delimited, contiguous words that's detected on a document page.     * /TABLE/ - A table that's detected on a document page. A table is any grid-based information with 2 or more rows or columns with a cell span of 1 row and 1 column each.      * /CELL/ - A cell within a detected table. The cell is the parent of the block that contains the text in the cell.     * /SELECTION_ELEMENT/ - A selectable element such as a radio button or checkbox that's detected on a document page. Use the value of @SelectionStatus@ to determine the status of the selection element.
block
    :: Block
block =
  Block'
    { _bColumnSpan = Nothing
    , _bText = Nothing
    , _bEntityTypes = Nothing
    , _bColumnIndex = Nothing
    , _bPage = Nothing
    , _bRowSpan = Nothing
    , _bSelectionStatus = Nothing
    , _bRowIndex = Nothing
    , _bConfidence = Nothing
    , _bRelationships = Nothing
    , _bGeometry = Nothing
    , _bId = Nothing
    , _bBlockType = Nothing
    }


-- | The number of columns that a table cell spans. @ColumnSpan@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
bColumnSpan :: Lens' Block (Maybe Natural)
bColumnSpan = lens _bColumnSpan (\ s a -> s{_bColumnSpan = a}) . mapping _Nat

-- | The word or line of text that's recognized by Amazon Textract.
bText :: Lens' Block (Maybe Text)
bText = lens _bText (\ s a -> s{_bText = a})

-- | The type of entity. The following can be returned:     * /KEY/ - An identifier for a field on the document.     * /VALUE/ - The field text. @EntityTypes@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
bEntityTypes :: Lens' Block [EntityType]
bEntityTypes = lens _bEntityTypes (\ s a -> s{_bEntityTypes = a}) . _Default . _Coerce

-- | The column in which a table cell appears. The first column position is 1. @ColumnIndex@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
bColumnIndex :: Lens' Block (Maybe Natural)
bColumnIndex = lens _bColumnIndex (\ s a -> s{_bColumnIndex = a}) . mapping _Nat

-- | The page in which a block was detected. @Page@ is returned by asynchronous operations. Page values greater than 1 are only returned for multi-page documents that are in PDF format. A scanned image (JPG/PNG), even if it contains multiple document pages, is always considered to be a single-page document and the value of @Page@ is always 1. Synchronous operations don't return @Page@ as every input document is considered to be a single-page document.
bPage :: Lens' Block (Maybe Natural)
bPage = lens _bPage (\ s a -> s{_bPage = a}) . mapping _Nat

-- | The number of rows that a table spans. @RowSpan@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
bRowSpan :: Lens' Block (Maybe Natural)
bRowSpan = lens _bRowSpan (\ s a -> s{_bRowSpan = a}) . mapping _Nat

-- | The selection status of a selectable element such as a radio button or checkbox.
bSelectionStatus :: Lens' Block (Maybe SelectionStatus)
bSelectionStatus = lens _bSelectionStatus (\ s a -> s{_bSelectionStatus = a})

-- | The row in which a table cell is located. The first row position is 1. @RowIndex@ isn't returned by @DetectDocumentText@ and @GetDocumentTextDetection@ .
bRowIndex :: Lens' Block (Maybe Natural)
bRowIndex = lens _bRowIndex (\ s a -> s{_bRowIndex = a}) . mapping _Nat

-- | The confidence that Amazon Textract has in the accuracy of the recognized text and the accuracy of the geometry points around the recognized text.
bConfidence :: Lens' Block (Maybe Double)
bConfidence = lens _bConfidence (\ s a -> s{_bConfidence = a})

-- | A list of child blocks of the current block. For example a LINE object has child blocks for each WORD block that's part of the line of text. There aren't Relationship objects in the list for relationships that don't exist, such as when the current block has no child blocks. The list size can be the following:     * 0 - The block has no child blocks.     * 1 - The block has child blocks.
bRelationships :: Lens' Block [Relationship]
bRelationships = lens _bRelationships (\ s a -> s{_bRelationships = a}) . _Default . _Coerce

-- | The location of the recognized text on the image. It includes an axis-aligned, coarse bounding box that surrounds the text, and a finer-grain polygon for more accurate spatial information.
bGeometry :: Lens' Block (Maybe Geometry)
bGeometry = lens _bGeometry (\ s a -> s{_bGeometry = a})

-- | The identifier for the recognized text. The identifier is only unique for a single operation.
bId :: Lens' Block (Maybe Text)
bId = lens _bId (\ s a -> s{_bId = a})

-- | The type of text that's recognized in a block. In text-detection operations, the following types are returned:     * /PAGE/ - Contains a list of the LINE Block objects that are detected on a document page.     * /WORD/ - A word detected on a document page. A word is one or more ISO basic Latin script characters that aren't separated by spaces.     * /LINE/ - A string of tab-delimited, contiguous words that's detected on a document page. In text analysis operations, the following types are returned:     * /PAGE/ - Contains a list of child Block objects that are detected on a document page.     * /KEY_VALUE_SET/ - Stores the KEY and VALUE Block objects for a field that's detected on a document page. Use the @EntityType@ field to determine if a KEY_VALUE_SET object is a KEY Block object or a VALUE Block object.      * /WORD/ - A word detected on a document page. A word is one or more ISO basic Latin script characters that aren't separated by spaces that's detected on a document page.     * /LINE/ - A string of tab-delimited, contiguous words that's detected on a document page.     * /TABLE/ - A table that's detected on a document page. A table is any grid-based information with 2 or more rows or columns with a cell span of 1 row and 1 column each.      * /CELL/ - A cell within a detected table. The cell is the parent of the block that contains the text in the cell.     * /SELECTION_ELEMENT/ - A selectable element such as a radio button or checkbox that's detected on a document page. Use the value of @SelectionStatus@ to determine the status of the selection element.
bBlockType :: Lens' Block (Maybe BlockType)
bBlockType = lens _bBlockType (\ s a -> s{_bBlockType = a})

instance FromJSON Block where
        parseJSON
          = withObject "Block"
              (\ x ->
                 Block' <$>
                   (x .:? "ColumnSpan") <*> (x .:? "Text") <*>
                     (x .:? "EntityTypes" .!= mempty)
                     <*> (x .:? "ColumnIndex")
                     <*> (x .:? "Page")
                     <*> (x .:? "RowSpan")
                     <*> (x .:? "SelectionStatus")
                     <*> (x .:? "RowIndex")
                     <*> (x .:? "Confidence")
                     <*> (x .:? "Relationships" .!= mempty)
                     <*> (x .:? "Geometry")
                     <*> (x .:? "Id")
                     <*> (x .:? "BlockType"))

instance Hashable Block where

instance NFData Block where

-- | The bounding box around the recognized text, key, value, table or table cell on a document page. The @left@ (x-coordinate) and @top@ (y-coordinate) are coordinates that represent the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0).
--
--
-- The @top@ and @left@ values returned are ratios of the overall document page size. For example, if the input image is 700 x 200 pixels, and the top-left coordinate of the bounding box is 350 x 50 pixels, the API returns a @left@ value of 0.5 (350/700) and a @top@ value of 0.25 (50/200).
--
-- The @width@ and @height@ values represent the dimensions of the bounding box as a ratio of the overall document page dimension. For example, if the document page size is 700 x 200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1.
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
-- * 'bbHeight' - The height of the bounding box as a ratio of the overall document page height.
--
-- * 'bbLeft' - The left coordinate of the bounding box as a ratio of overall document page width.
--
-- * 'bbWidth' - The width of the bounding box as a ratio of the overall document page width.
--
-- * 'bbTop' - The top coordinate of the bounding box as a ratio of overall document page height.
boundingBox
    :: BoundingBox
boundingBox =
  BoundingBox'
    { _bbHeight = Nothing
    , _bbLeft = Nothing
    , _bbWidth = Nothing
    , _bbTop = Nothing
    }


-- | The height of the bounding box as a ratio of the overall document page height.
bbHeight :: Lens' BoundingBox (Maybe Double)
bbHeight = lens _bbHeight (\ s a -> s{_bbHeight = a})

-- | The left coordinate of the bounding box as a ratio of overall document page width.
bbLeft :: Lens' BoundingBox (Maybe Double)
bbLeft = lens _bbLeft (\ s a -> s{_bbLeft = a})

-- | The width of the bounding box as a ratio of the overall document page width.
bbWidth :: Lens' BoundingBox (Maybe Double)
bbWidth = lens _bbWidth (\ s a -> s{_bbWidth = a})

-- | The top coordinate of the bounding box as a ratio of overall document page height.
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

-- | The input document, either as bytes or as an S3 object.
--
--
-- You pass image bytes to an Amazon Textract API operation by using the @Bytes@ property. For example, you would use the @Bytes@ property to pass a document loaded from a local file system. Image bytes passed by using the @Bytes@ property must be base64 encoded. Your code might not need to encode document file bytes if you're using an AWS SDK to call Amazon Textract API operations.
--
-- You pass images stored in an S3 bucket to an Amazon Textract API operation by using the @S3Object@ property. Documents stored in an S3 bucket don't need to be base64 encoded.
--
-- The AWS Region for the S3 bucket that contains the S3 object must match the AWS Region that you use for Amazon Textract operations.
--
-- If you use the AWS CLI to call Amazon Textract operations, passing image bytes using the Bytes property isn't supported. You must first upload the document to an Amazon S3 bucket, and then call the operation using the S3Object property.
--
-- For Amazon Textract to process an S3 object, the user must have permission to access the S3 object.
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
-- * 'dS3Object' - Identifies an S3 object as the document source. The maximum size of a document stored in an S3 bucket is 5 MB.
--
-- * 'dBytes' - A blob of base-64 encoded documents bytes. The maximum size of a document that's provided in a blob of bytes is 5 MB. The document bytes must be in PNG or JPG format. If you are using an AWS SDK to call Amazon Textract, you might not need to base64-encode image bytes passed using the @Bytes@ field. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
document
    :: Document
document = Document' {_dS3Object = Nothing, _dBytes = Nothing}


-- | Identifies an S3 object as the document source. The maximum size of a document stored in an S3 bucket is 5 MB.
dS3Object :: Lens' Document (Maybe S3Object)
dS3Object = lens _dS3Object (\ s a -> s{_dS3Object = a})

-- | A blob of base-64 encoded documents bytes. The maximum size of a document that's provided in a blob of bytes is 5 MB. The document bytes must be in PNG or JPG format. If you are using an AWS SDK to call Amazon Textract, you might not need to base64-encode image bytes passed using the @Bytes@ field. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
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

-- | The Amazon S3 bucket that contains the document to be processed. It's used by asynchronous operations such as 'StartDocumentTextDetection' .
--
--
-- The input document can be an image file in JPG or PNG format. It can also be a file in PDF format.
--
--
-- /See:/ 'documentLocation' smart constructor.
newtype DocumentLocation = DocumentLocation'
  { _dlS3Object :: Maybe S3Object
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlS3Object' - The Amazon S3 bucket that contains the input document.
documentLocation
    :: DocumentLocation
documentLocation = DocumentLocation' {_dlS3Object = Nothing}


-- | The Amazon S3 bucket that contains the input document.
dlS3Object :: Lens' DocumentLocation (Maybe S3Object)
dlS3Object = lens _dlS3Object (\ s a -> s{_dlS3Object = a})

instance Hashable DocumentLocation where

instance NFData DocumentLocation where

instance ToJSON DocumentLocation where
        toJSON DocumentLocation'{..}
          = object
              (catMaybes [("S3Object" .=) <$> _dlS3Object])

-- | Information about the input document.
--
--
--
-- /See:/ 'documentMetadata' smart constructor.
newtype DocumentMetadata = DocumentMetadata'
  { _dmPages :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmPages' - The number of pages detected in the document.
documentMetadata
    :: DocumentMetadata
documentMetadata = DocumentMetadata' {_dmPages = Nothing}


-- | The number of pages detected in the document.
dmPages :: Lens' DocumentMetadata (Maybe Natural)
dmPages = lens _dmPages (\ s a -> s{_dmPages = a}) . mapping _Nat

instance FromJSON DocumentMetadata where
        parseJSON
          = withObject "DocumentMetadata"
              (\ x -> DocumentMetadata' <$> (x .:? "Pages"))

instance Hashable DocumentMetadata where

instance NFData DocumentMetadata where

-- | Information about where a recognized text, key, value, table, or table cell is located on a document page.
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
-- * 'gBoundingBox' - An axis-aligned coarse representation of the location of the recognized text on the document page.
--
-- * 'gPolygon' - Within the bounding box, a fine-grained polygon around the recognized text.
geometry
    :: Geometry
geometry = Geometry' {_gBoundingBox = Nothing, _gPolygon = Nothing}


-- | An axis-aligned coarse representation of the location of the recognized text on the document page.
gBoundingBox :: Lens' Geometry (Maybe BoundingBox)
gBoundingBox = lens _gBoundingBox (\ s a -> s{_gBoundingBox = a})

-- | Within the bounding box, a fine-grained polygon around the recognized text.
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

-- | The Amazon Simple Notification Service (Amazon SNS) topic to which Amazon Textract publishes the completion status of an asynchronous document operation, such as 'StartDocumentTextDetection' .
--
--
--
-- /See:/ 'notificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { _ncSNSTopicARN :: !Text
  , _ncRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncSNSTopicARN' - The Amazon SNS topic that Amazon Textract posts the completion status to.
--
-- * 'ncRoleARN' - The Amazon Resource Name (ARN) of an IAM role that gives Amazon Textract publishing permissions to the Amazon SNS topic.
notificationChannel
    :: Text -- ^ 'ncSNSTopicARN'
    -> Text -- ^ 'ncRoleARN'
    -> NotificationChannel
notificationChannel pSNSTopicARN_ pRoleARN_ =
  NotificationChannel' {_ncSNSTopicARN = pSNSTopicARN_, _ncRoleARN = pRoleARN_}


-- | The Amazon SNS topic that Amazon Textract posts the completion status to.
ncSNSTopicARN :: Lens' NotificationChannel Text
ncSNSTopicARN = lens _ncSNSTopicARN (\ s a -> s{_ncSNSTopicARN = a})

-- | The Amazon Resource Name (ARN) of an IAM role that gives Amazon Textract publishing permissions to the Amazon SNS topic.
ncRoleARN :: Lens' NotificationChannel Text
ncRoleARN = lens _ncRoleARN (\ s a -> s{_ncRoleARN = a})

instance Hashable NotificationChannel where

instance NFData NotificationChannel where

instance ToJSON NotificationChannel where
        toJSON NotificationChannel'{..}
          = object
              (catMaybes
                 [Just ("SNSTopicArn" .= _ncSNSTopicARN),
                  Just ("RoleArn" .= _ncRoleARN)])

-- | The X and Y coordinates of a point on a document page. The X and Y values returned are ratios of the overall document page size. For example, if the input document is 700 x 200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the document page.
--
--
-- An array of @Point@ objects, @Polygon@ , is returned by 'DetectDocumentText' . @Polygon@ represents a fine-grained polygon around detected text. For more information, see Geometry in the Amazon Textract Developer Guide.
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

-- | Information about how blocks are related to each other. A @Block@ object contains 0 or more @Relation@ objects in a list, @Relationships@ . For more information, see 'Block' .
--
--
-- The @Type@ element provides the type of the relationship for all blocks in the @IDs@ array.
--
--
-- /See:/ 'relationship' smart constructor.
data Relationship = Relationship'
  { _rIds  :: !(Maybe [Text])
  , _rType :: !(Maybe RelationshipType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Relationship' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rIds' - An array of IDs for related blocks. You can get the type of the relationship from the @Type@ element.
--
-- * 'rType' - The type of relationship that the blocks in the IDs array have with the current block. The relationship can be @VALUE@ or @CHILD@ .
relationship
    :: Relationship
relationship = Relationship' {_rIds = Nothing, _rType = Nothing}


-- | An array of IDs for related blocks. You can get the type of the relationship from the @Type@ element.
rIds :: Lens' Relationship [Text]
rIds = lens _rIds (\ s a -> s{_rIds = a}) . _Default . _Coerce

-- | The type of relationship that the blocks in the IDs array have with the current block. The relationship can be @VALUE@ or @CHILD@ .
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

-- | The S3 bucket name and file name that identifies the document.
--
--
-- The AWS Region for the S3 bucket that contains the document must match the Region that you use for Amazon Textract operations.
--
-- For Amazon Textract to process a file in an S3 bucket, the user must have permission to access the S3 bucket and file.
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
-- * 'soBucket' - The name of the S3 bucket.
--
-- * 'soName' - The file name of the input document. It must be an image file (.JPG or .PNG format). Asynchronous operations also support PDF files.
--
-- * 'soVersion' - If the bucket has versioning enabled, you can specify the object version.
s3Object
    :: S3Object
s3Object =
  S3Object' {_soBucket = Nothing, _soName = Nothing, _soVersion = Nothing}


-- | The name of the S3 bucket.
soBucket :: Lens' S3Object (Maybe Text)
soBucket = lens _soBucket (\ s a -> s{_soBucket = a})

-- | The file name of the input document. It must be an image file (.JPG or .PNG format). Asynchronous operations also support PDF files.
soName :: Lens' S3Object (Maybe Text)
soName = lens _soName (\ s a -> s{_soName = a})

-- | If the bucket has versioning enabled, you can specify the object version.
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

-- | A warning about an issue that occurred during asynchronous text analysis ('StartDocumentAnalysis' ) or asynchronous document-text detection ('StartDocumentTextDetection' ).
--
--
--
-- /See:/ 'warning' smart constructor.
data Warning = Warning'
  { _wPages     :: !(Maybe [Nat])
  , _wErrorCode :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Warning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wPages' - A list of the pages that the warning applies to.
--
-- * 'wErrorCode' - The error code for the warning.
warning
    :: Warning
warning = Warning' {_wPages = Nothing, _wErrorCode = Nothing}


-- | A list of the pages that the warning applies to.
wPages :: Lens' Warning [Natural]
wPages = lens _wPages (\ s a -> s{_wPages = a}) . _Default . _Coerce

-- | The error code for the warning.
wErrorCode :: Lens' Warning (Maybe Text)
wErrorCode = lens _wErrorCode (\ s a -> s{_wErrorCode = a})

instance FromJSON Warning where
        parseJSON
          = withObject "Warning"
              (\ x ->
                 Warning' <$>
                   (x .:? "Pages" .!= mempty) <*> (x .:? "ErrorCode"))

instance Hashable Warning where

instance NFData Warning where
