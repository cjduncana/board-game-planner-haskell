module Types.NonEmptyText (NonEmptyText, fromText, toByteString, toText) where

import Control.Category ((>>>))
import Control.Exception.Base (Exception)
import Data.ByteString (ByteString)
import qualified Data.Maybe as Maybe
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Database.MySQL.Simple.Param (Param(render))
import Database.MySQL.Simple.Result (Result(convert))
import Text.XML.Decode.DecodeCursor (DecodeCursor)
import qualified Text.XML.Decode.DecodeCursor as DecodeCursor

import qualified Types.Utils as Utils

data NonEmptyText = NonEmptyText Char Text deriving (Eq, Ord)

instance DecodeCursor NonEmptyText where
  decode = DecodeCursor.parseCursor fromText

data NonEmptyTextException
  = NotANonEmptyText
  | NotAText
  deriving (Exception, Show)

instance GQLScalar NonEmptyText where
  parseValue (String value) = fromText value
  parseValue _ = Left "Value should be of type String"
  serialize = toText >>> String

instance GQLType NonEmptyText where
  type KIND NonEmptyText = SCALAR

instance Param NonEmptyText where
  render = toText >>> render

instance Result NonEmptyText where
  convert field value =
    Maybe.fromMaybe
      (Utils.conversionFailed field "NonEmptyText" "could not parse")
      (fromText' (convert field value))

instance Show NonEmptyText where
  show = toText >>> show

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
