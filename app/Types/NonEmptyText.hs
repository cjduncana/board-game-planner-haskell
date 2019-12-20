module Types.NonEmptyText (NonEmptyText, fromText, toByteString, toText) where

import Control.Category ((>>>))
import Control.Exception.Base (Exception, SomeException(SomeException))
import Data.ByteString (ByteString)
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField (FromField(fromField), fieldData)
import Database.SQLite.Simple.Ok (Ok(Errors, Ok))
import Database.SQLite.Simple.ToField (ToField(toField))
import Text.XML.Decode.DecodeCursor (DecodeCursor)
import qualified Text.XML.Decode.DecodeCursor as DecodeCursor

data NonEmptyText = NonEmptyText Char Text deriving (Eq, Ord)

instance DecodeCursor NonEmptyText where
  decode = DecodeCursor.parseCursor fromText

data NonEmptyTextException
  = NotANonEmptyText
  | NotAText
  deriving (Exception, Show)

instance FromField NonEmptyText where
  fromField field =
    case fieldData field of
      SQLText text -> maybe (Errors [SomeException NotANonEmptyText]) Ok (fromText' text)
      _ -> Errors [SomeException NotAText]

instance GQLScalar NonEmptyText where
  parseValue (String value) = fromText value
  parseValue _ = Left "Value should be of type String"
  serialize = toText >>> String

instance GQLType NonEmptyText where
  type KIND NonEmptyText = SCALAR

instance Show NonEmptyText where
  show = toText >>> show

instance ToField NonEmptyText where
  toField = toText >>> SQLText

toByteString :: NonEmptyText -> ByteString
toByteString = toText >>> Encoding.encodeUtf8

toText :: NonEmptyText -> Text
toText = uncons >>> uncurry Text.cons

fromText :: Text -> Either Text NonEmptyText
fromText =
  fromText' >>> maybe (Left "Value can't have an empty String") Right

fromText' :: Text -> Maybe NonEmptyText
fromText' =
  Text.strip
   >>> Text.uncons
   >>> fmap (uncurry NonEmptyText)

uncons :: NonEmptyText -> (Char, Text)
uncons (NonEmptyText h t) = (h, t)
