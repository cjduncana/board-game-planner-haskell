module Types.EmailAddress (EmailAddress) where

import Control.Category ((>>>))
import Control.Exception.Base (Exception, SomeException(SomeException))
import Data.Function ((&))
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
import qualified Text.Email.Validate as Email

newtype EmailAddress = EmailAddress Email.EmailAddress

data EmailAddressException
  = NotAEmailAddress
  | NotAText
  deriving (Exception, Show)

instance FromField EmailAddress where
  fromField field =
    case fieldData field of
      SQLText text -> either (const $ Errors [SomeException NotAEmailAddress]) Ok (fromText text)
      _ -> Errors [SomeException NotAText]

instance GQLScalar EmailAddress where
  parseValue (String value) = fromText value
  parseValue _ = Left "Value should be of type String"
  serialize = toText >>> String

instance GQLType EmailAddress where
  type KIND EmailAddress = SCALAR

instance ToField EmailAddress where
  toField = toText >>> SQLText

fromText :: Text -> Either Text EmailAddress
fromText =
  Encoding.encodeUtf8
    >>> Email.validate
    >>> either (Text.pack >>> Left) (EmailAddress >>> Right)

toText :: EmailAddress -> Text
toText (EmailAddress email) =
  Email.toByteString email
    & Encoding.decodeUtf8
