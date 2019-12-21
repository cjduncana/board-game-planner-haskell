module Types.JWT (JWT, decodeAndVerify) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Maybe as Maybe
import Data.Text (Text)
import qualified Web.JWT as Web

import Types.Time (Time)
import qualified Types.Time as Time

type JWT = Web.JWT Web.VerifiedJWT

data JWTError
  = Missing
  | Expired
  | Invalid

instance Show JWTError where
  show Missing = "Missing credentials"
  show Expired = "Expired credentials"
  show Invalid = "Invalid credentials"

decodeAndVerify :: Web.Signer -> Time -> Text -> Either JWTError JWT
decodeAndVerify signer now encodedToken = do
  decodedToken <- maybe (Left Missing) Right (Web.decode encodedToken)
  if hasExpired now decodedToken
    then
      Left Expired
    else
      maybe (Left Invalid) Right (Web.verify signer decodedToken)

hasExpired :: Time -> Web.JWT r -> Bool
hasExpired now =
  Web.claims
    >>> Web.exp
    >>> (=<<) beforeToday
    >>> Maybe.fromMaybe True

  where
    beforeToday :: Web.NumericDate -> Maybe Bool
    beforeToday expDate =
      Time.toNumericDate now
        & fmap (> expDate)
