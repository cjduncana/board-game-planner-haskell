module Types.Utils (conversionFailed) where

import Control.Category ((>>>))
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as ByteString
import Database.MySQL.Base.Types (Field(fieldName, fieldType))
import Database.MySQL.Simple.Result (ResultError(ConversionFailed))

conversionFailed :: Field -> String -> String -> a
conversionFailed f s =
  ConversionFailed
    (show (fieldType f))
    s
    (ByteString.unpack (fieldName f))
    >>> Exception.throw
