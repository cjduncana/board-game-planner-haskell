-- For more information about How to Implement Proper Password Strength Controls, visit:
-- https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Authentication_Cheat_Sheet.md#implement-proper-password-strength-controls

module Types.Password (Password, toByteString) where

import Control.Category ((>>>))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Prelude
    ( Either(Left, Right)
    , Int
    , Show(show)
    , String
    , either
    , otherwise
    , ($)
    , (<)
    , (<>)
    , (>)
    )

newtype Password = Password Text

instance GQLScalar Password where
  parseValue (String value) =
    validatePassword value
      & either (show >>> Text.pack >>> Left) Right
  parseValue _ = Left "Value should be of type String"

  serialize (Password value) = String value

instance GQLType Password where
  type KIND Password = SCALAR

data Error
  = TooShort Int
  | TooLong Int

instance Show Error where
  show (TooShort length) =
    "Password should be at least " <> show minLength <> " characters long. " <> textLength length
  show (TooLong length) =
    "Password should be at most " <> show maxLength <> " characters long. " <> textLength length

instance Show Password where
  show (Password text) = show text

textLength :: Int -> String
textLength length =
  "Your text is " <> show length <> " characters long."

toByteString :: Password -> ByteString
toByteString (Password value) = Encoding.encodeUtf8 value

validatePassword :: Text -> Either Error Password
validatePassword value
  | length < minLength = Left $ TooShort length
  | length > maxLength = Left $ TooLong length
  | otherwise = Right $ Password value
  where
    length = Text.length value

minLength :: Int
minLength = 8

maxLength :: Int
maxLength = 128
