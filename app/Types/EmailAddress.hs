module Types.EmailAddress (EmailAddress) where

import Control.Category ((>>>))
import Control.Exception.Base (Exception)
import Data.Function ((&))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Database.MySQL.Simple.Param (Param(render))
import Database.MySQL.Simple.Result (Result(convert))
import qualified Text.Email.Validate as Email

import qualified Types.Utils as Utils

newtype EmailAddress = EmailAddress Email.EmailAddress

data EmailAddressException
  = NotAEmailAddress
  | NotAText
  deriving (Exception, Show)

fromText :: Text -> Either Text EmailAddress
fromText =
  Encoding.encodeUtf8
    >>> Email.validate
    >>> either (Text.pack >>> Left) (EmailAddress >>> Right)

toText :: EmailAddress -> Text
toText (EmailAddress email) =
  Email.toByteString email
    & Encoding.decodeUtf8

instance GQLScalar EmailAddress where
  parseValue (String value) = fromText value
  parseValue _ = Left "Value should be of type String"
  serialize = toText >>> String

instance GQLType EmailAddress where
  type KIND EmailAddress = SCALAR

instance Param EmailAddress where
  render (EmailAddress email) =
    Email.toByteString email
      & render

instance Result EmailAddress where
  convert field =
    convert field
      >>> Email.validate
      >>> either (Utils.conversionFailed field "EmailAddress") EmailAddress

instance Show EmailAddress where
  show (EmailAddress email) = show email
