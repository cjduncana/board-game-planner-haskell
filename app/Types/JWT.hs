module Types.JWT (JWT, decodeAndVerify) where

import Data.Text (Text)
import qualified Web.JWT as Web

type JWT = Web.JWT Web.VerifiedJWT

data JWTError
  = Missing
  | Expired
  | Invalid

instance Show JWTError where
  show Missing = "Missing credentials"
  show Expired = "Expired credentials"
  show Invalid = "Invalid credentials"

decodeAndVerify :: Web.Signer -> Text -> Either JWTError JWT
decodeAndVerify signer encodedToken = do
  decodedToken <- maybe (Left Missing) Right (Web.decode encodedToken)
  -- TODO: Add expiring logic
  maybe (Left Invalid) Right (Web.verify signer decodedToken)
