module Types.PositiveInteger (PositiveInteger, fromText) where

import Control.Category ((>>>))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(Int))
import Data.Text (Text)
import qualified Text.XML.Decode.Parsers as Parsers

newtype PositiveInteger = PositiveInteger Int

data PositiveIntegerError
  = IntegerIsZero
  | IntegerIsNegative

fromText :: Text -> Either Text PositiveInteger
fromText text = do
  integer <- Parsers.parseInt text
  fromEitherPositiveIntegerErrorToText $ fromInt integer

fromEitherPositiveIntegerErrorToText :: Either PositiveIntegerError a -> Either Text a
fromEitherPositiveIntegerErrorToText =
  either (positiveIntegerErrorToText >>> Left) Right

positiveIntegerErrorToText :: PositiveIntegerError -> Text
positiveIntegerErrorToText IntegerIsZero = "Integer shouldn't be zero"
positiveIntegerErrorToText IntegerIsNegative = "Integer shouldn't be negative"

fromInt :: Int -> Either PositiveIntegerError PositiveInteger
fromInt integer
  | integer == 0 = Left IntegerIsZero
  | integer < 0 = Left IntegerIsNegative
  | otherwise = Right $ PositiveInteger integer

toInt :: PositiveInteger -> Int
toInt (PositiveInteger integer) = integer

instance GQLScalar PositiveInteger where
  parseValue (Int value) =
    case fromInt value of
      Left e ->  Left $ positiveIntegerErrorToText e
      Right posInt -> Right posInt
  parseValue _ = Left "Value should be of type Int"
  serialize = toInt >>> Int

instance GQLType PositiveInteger where
  type KIND PositiveInteger = SCALAR

instance Show PositiveInteger where
  show (PositiveInteger integer) = show integer
