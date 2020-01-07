module Types.HashedPassword (HashedPassword, encodePassword, verifyPassword) where

import Control.Category ((>>>))
import Control.Exception.Base (Exception)
import Crypto.Argon2 (Argon2Status)
import qualified Crypto.Argon2 as Argon2
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as Short
import Database.MySQL.Simple.Param (Param(render))
import Database.MySQL.Simple.Result (Result(convert))

import Types.Password (Password)
import qualified Types.Password as Password

newtype HashedPassword = HashedPassword ShortText

data HashedPasswordException
  = NotAText
  deriving (Exception, Show)

instance Param HashedPassword where
  render (HashedPassword hashedPassword) =
    Short.toText hashedPassword
      & render

instance Result HashedPassword where
  convert field =
    convert field
      >>> Short.pack
      >>> HashedPassword

encodePassword :: Password -> ByteString -> Either String HashedPassword
encodePassword password =
  Argon2.hashEncoded Argon2.defaultHashOptions (Password.toByteString password)
    >>> either (show >>> Left) (HashedPassword >>> Right)

verifyPassword :: HashedPassword -> Password -> Argon2Status
verifyPassword (HashedPassword hashedPassword) password =
  Argon2.verifyEncoded hashedPassword $ Password.toByteString password
