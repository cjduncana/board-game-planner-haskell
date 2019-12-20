module Types.HashedPassword (HashedPassword, encodePassword, verifyPassword) where

import Control.Category ((>>>))
import Control.Exception.Base (Exception, SomeException(SomeException))
import Crypto.Argon2 (Argon2Status)
import qualified Crypto.Argon2 as Argon2
import Data.ByteString (ByteString)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as Short
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField (FromField(fromField), fieldData)
import Database.SQLite.Simple.Ok (Ok(Errors, Ok))
import Database.SQLite.Simple.ToField (ToField(toField))

import Types.Password (Password)
import qualified Types.Password as Password

newtype HashedPassword = HashedPassword ShortText

data HashedPasswordException
  = NotAText
  deriving (Exception, Show)

instance FromField HashedPassword where
  fromField field =
    case fieldData field of
      SQLText text -> Ok $ HashedPassword $ Short.fromText text
      _ -> Errors [SomeException NotAText]

instance ToField HashedPassword where
  toField (HashedPassword hashedPassword) =
    (Short.toText >>> SQLText) hashedPassword

encodePassword :: Password -> ByteString -> Either String HashedPassword
encodePassword password =
  Argon2.hashEncoded Argon2.defaultHashOptions (Password.toByteString password)
    >>> either (show >>> Left) (HashedPassword >>> Right)

verifyPassword :: HashedPassword -> Password -> Argon2Status
verifyPassword (HashedPassword hashedPassword) password =
  Argon2.verifyEncoded hashedPassword $ Password.toByteString password
